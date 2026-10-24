# Run every step in pipeline order and log how the data changes shape.
#
#     Rscript R/trace_pipeline.R    (from the project root)
#
# Everything is written under trace/<run_id>/: the SQLite database, the summary
# tables and the log. cache/, output/ and analysis.md are never touched -- the
# trace calls clean_retail() directly rather than cleaned_lines(), which would
# write the cache, and hands build_sqlite() a path of its own. At the end each
# table is compared with the committed copy in output/ (as of HEAD), so the log
# doubles as a reproducibility check. The report is not knitted; the last step
# says what that leaves out. About 20 seconds.
#
#     trace.log          full log: per step, input and output shape, column
#                        types, NAs, ranges, sample rows, what changed
#     data_lineage.md    one table, one row per step

source(file.path("R", "functions.R"))

TOTAL <- 5
lineage <- list()
log_con <- NULL

# ---------------------------------------------------------------- formatting

log_line <- function(...) {
  lines <- strsplit(paste0(...), "\n", fixed = TRUE)[[1]]
  if (!length(lines)) lines <- ""
  for (line in lines) {
    out <- paste0(format(Sys.time(), "%H:%M:%S"), "  ", line)
    cat(out, "\n", sep = "")
    writeLines(out, log_con)
  }
}

header <- function(n, name, fn, what) {
  log_line("")
  log_line(strrep("=", 78))
  log_line(sprintf("[%d/%d] %s   (%s)   %s", n, TOTAL, name, fn, what))
  log_line(strrep("=", 78))
}

fmt_int <- function(x) format(x, big.mark = ",", scientific = FALSE, trim = TRUE)

fmt_val <- function(x) {
  if (inherits(x, c("Date", "POSIXct"))) return(format(x))
  if (x == round(x)) return(format(x, scientific = FALSE, trim = TRUE))  # ids, years in full
  trimws(formatC(x, digits = 4, format = "g", big.mark = ","))
}

#' Shape, memory, per-column class / NAs / range, and a few rows.
log_frame <- function(label, df, sample = 3, max_cols = 10) {
  mem <- as.numeric(utils::object.size(df)) / 2^20
  log_line(sprintf("  %s: %s 行 x %d 列, 内存 %.1f MB", label, fmt_int(nrow(df)), ncol(df), mem))
  for (name in names(df)) {
    col <- df[[name]]
    info <- sprintf("%-22s 空值 %8s", paste(class(col), collapse = "/"), fmt_int(sum(is.na(col))))
    if (is.logical(col)) {
      info <- paste0(info, "  TRUE ", fmt_int(sum(col, na.rm = TRUE)))
    } else if (is.numeric(col) || inherits(col, c("Date", "POSIXct"))) {
      if (any(!is.na(col))) {
        r <- range(col, na.rm = TRUE)
        info <- paste0(info, "  范围 [", fmt_val(r[1]), ", ", fmt_val(r[2]), "]")
      }
    } else {
      info <- paste0(info, "  ", fmt_int(n_distinct(col, na.rm = TRUE)), " 种取值")
    }
    log_line(sprintf("      %-24s %s", name, info))
  }
  shown <- as.data.frame(utils::head(df[, seq_len(min(max_cols, ncol(df))), drop = FALSE], sample))
  shown[] <- lapply(shown, function(v) {
    if (is.character(v)) ifelse(nchar(v) > 22, paste0(substr(v, 1, 19), "..."), v) else v
  })
  extra <- if (ncol(df) > max_cols) sprintf("  (另有 %d 列未显示)", ncol(df) - max_cols) else ""
  log_line(sprintf("      前 %d 行%s:", min(sample, nrow(df)), extra))
  log_line(paste0("      | ", utils::capture.output(print(shown)), collapse = "\n"))
}

#' A small frame in full: the audit, query plans.
log_table <- function(df) {
  log_line(paste0(
    "      | ", utils::capture.output(print(as.data.frame(df), row.names = FALSE)),
    collapse = "\n"
  ))
}

change <- function(text) log_line("  -> 变化: ", text)

record <- function(step, fn, input, output, rows, what) {
  lineage[[length(lineage) + 1]] <<- list(
    step = step, fn = fn, input = input, output = output, rows = rows, change = what
  )
}

# Runs one step and logs how long it took. Anything the step itself says, as a
# message or a warning, goes into the trace too, marked, instead of only to the
# console.
timed <- function(f, ...) {
  t0 <- proc.time()[["elapsed"]]
  result <- withCallingHandlers(
    f(...),
    message = function(m) {
      log_line("[R] ", trimws(conditionMessage(m)))
      invokeRestart("muffleMessage")
    },
    warning = function(w) {
      log_line("[R] 警告: ", conditionMessage(w))
      invokeRestart("muffleWarning")
    }
  )
  log_line(sprintf("  耗时 %.1f s", proc.time()[["elapsed"]] - t0))
  result
}

git <- function(...) {
  out <- suppressWarnings(system2("git", c(...), stdout = TRUE, stderr = FALSE))
  if (is.null(attr(out, "status"))) out else NULL
}

#' Short commit, -dirty when the tree has changes: the code this trace ran.
git_tag <- function() {
  sha <- git("rev-parse", "--short", "HEAD")
  paste0(if (length(sha)) sha[1] else "nogit", if (length(git("status", "--porcelain"))) "-dirty" else "")
}

# ---------------------------------------------------------------- steps

step_config <- function() {
  header(0, "config", "R/functions.R", "所有步骤共用的常量和环境")
  log_line("  ", R.version.string)
  pkgs <- c("readr", "dplyr", "lubridate", "DBI", "RSQLite", "zoo", "forecast")
  log_line("  包版本 (renv.lock): ", paste(
    sprintf("%s %s", pkgs, vapply(pkgs, function(p) as.character(packageVersion(p)), "")),
    collapse = ", "
  ))
  log_line("  raw_csv_path(): ", raw_csv_path())
  log_line("  DUPLICATE_KEY: ", paste(DUPLICATE_KEY, collapse = ", "))
  log_line("  SERVICE_CODES: ", paste(SERVICE_CODES, collapse = ", "))
  log_line("  sql/queries.sql: ", paste(names(read_queries()), collapse = ", "))
}

step_load <- function() {
  header(1, "load", "read_retail_raw()", "读 CSV, 每列指定类型, 列改名")
  path <- raw_csv_path()
  pinned <- sub(
    '.*"([0-9a-f]{64})".*', "\\1",
    grep("^EXPECTED_SHA256", readLines(proj_path("R", "get_data.R")), value = TRUE)
  )
  digest <- file_sha256(path)
  log_line(sprintf(
    "  %s: %s 字节, sha256 %s... -> %s",
    basename(path), fmt_int(file.size(path)), substr(digest, 1, 16),
    if (identical(digest, pinned)) "与 R/get_data.R 固定的一致" else "与 R/get_data.R 固定的不一致"
  ))
  raw <- timed(read_retail_raw)
  log_frame("read_retail_raw() 输出", raw)
  change(paste(
    "一行不删。8 列按 col_types 读: Quantity / UnitPrice 是 double, 其余 (包括",
    "InvoiceDate 和 CustomerID) 都是字符; 列名改成小写下划线。时间还是字符串"
  ))
  record("load", "read_retail_raw()", sprintf("%s (%.0f MB)", basename(path), file.size(path) / 2^20),
         sprintf("raw %s 行 x %d 列", fmt_int(nrow(raw)), ncol(raw)), "x1 (不删行)",
         "按列定类型; 列改名")
  raw
}

step_clean <- function(raw) {
  header(2, "clean", "clean_retail()", "规则依次执行, 每条记下删了多少行; 退货单和销售一对一抵消")
  cleaned <- timed(clean_retail, raw)
  audit <- cleaned$audit |>
    mutate(
      rows_left = cleaned$rows_in - cumsum(rows_dropped),
      share_of_input = sprintf("%.2f%%", 100 * share_of_input)
    )
  log_line("  按顺序执行的规则 (每条只数前面的规则留下来的行, 所以可以相加):")
  log_table(audit)
  lines <- cleaned$lines
  log_frame("clean_retail() 输出 lines", lines)
  dropped <- cleaned$rows_in - cleaned$rows_out
  log_line(sprintf(
    "  核对: rows_in %s - 删掉 %s = %s vs rows_out %s -> %s",
    fmt_int(cleaned$rows_in), fmt_int(sum(cleaned$audit$rows_dropped)),
    fmt_int(cleaned$rows_in - sum(cleaned$audit$rows_dropped)), fmt_int(cleaned$rows_out),
    if (cleaned$rows_in - sum(cleaned$audit$rows_dropped) == cleaned$rows_out) "一致" else "不一致"
  ))
  log_line("  退货单抵消 (cleaned$netting):")
  for (name in names(cleaned$netting)) {
    v <- cleaned$netting[[name]]
    shown <- if (grepl("share", name)) {
      sprintf("%.2f%%", 100 * v)
    } else if (grepl("value|credits$", name)) {
      paste0("GBP ", formatC(v, format = "f", digits = 2, big.mark = ","))
    } else {
      fmt_int(v)
    }
    log_line(sprintf("      %-32s %s", name, shown))
  }
  change(sprintf(paste(
    "%s 行 -> %s 行 (删 %s 行, %.1f%%)。invoice_date 字符串 -> invoice_ts (POSIXct,",
    "mdy_hm 解析); 新增 revenue = quantity x unit_price; 退货单 (C 开头) 整行去掉, 它们",
    "一对一抵消掉的 %s 行销售也去掉"
  ), fmt_int(cleaned$rows_in), fmt_int(cleaned$rows_out), fmt_int(dropped),
  100 * dropped / cleaned$rows_in, fmt_int(cleaned$netting$matched_lines)))
  record("clean", "clean_retail()", sprintf("raw %s 行", fmt_int(cleaned$rows_in)),
         sprintf("lines %s 行 x %d 列", fmt_int(nrow(lines)), ncol(lines)),
         sprintf("-%s (%.1f%%)", fmt_int(dropped), 100 * dropped / cleaned$rows_in),
         sprintf("%d 条规则依次删行; 退货单抵消; 加 invoice_ts, revenue", nrow(audit)))
  cleaned
}

step_sqlite <- function(lines, out) {
  header(3, "sqlite", "build_sqlite()", "干净的行写进 SQLite, 时间存成 ISO 文本")
  db <- file.path(out, "retail.sqlite")
  con <- timed(build_sqlite, lines, db_path = db)
  n <- dbGetQuery(con, "SELECT count(*) AS n FROM retail_lines")$n
  log_line(sprintf("  retail_lines: %s 行, %s (%.1f MB)", fmt_int(n), db, file.size(db) / 2^20))
  log_line("  SQLite 里的列类型:")
  log_table(dbGetQuery(con, "PRAGMA table_info(retail_lines)")[, c("name", "type")])
  log_line("  前 3 行:")
  log_table(dbGetQuery(con, "SELECT * FROM retail_lines LIMIT 3"))
  log_line(sprintf("  核对: %s 行 vs lines %s 行 -> %s",
                   fmt_int(n), fmt_int(nrow(lines)), if (n == nrow(lines)) "一致" else "不一致"))
  change(paste(
    "行数不变。POSIXct 时间转成 '%Y-%m-%d %H:%M:%S' 文本 (SQLite 没有时间类型,",
    "strftime() 直接按这个文本取月和日); 数值列存成 REAL, 字符列存成 TEXT"
  ))
  record("sqlite", "build_sqlite()", sprintf("lines %s 行", fmt_int(nrow(lines))),
         sprintf("retail_lines %s 行", fmt_int(n)), "不变", "POSIXct -> ISO 文本")
  con
}

step_queries <- function(con, cleaned, out) {
  header(4, "queries", "read_queries() + run_queries()", "sql/queries.sql 里的命名查询, 每个一张汇总表")
  queries <- read_queries()
  results <- timed(run_queries, con, queries)
  dbDisconnect(con)
  write_csv(cleaned$audit, file.path(out, "cleaning_audit.csv"))
  for (name in names(results)) {
    write_csv(results[[name]], file.path(out, paste0(name, ".csv")))
    log_line("")
    log_line("  -- name: ", name)
    sql <- strsplit(trimws(queries[[name]]), "\n", fixed = TRUE)[[1]]
    log_line(paste0("      sql> ", sql, collapse = "\n"))
    log_frame(name, results[[name]], sample = min(nrow(results[[name]]), 13), max_cols = 12)
  }
  total <- sum(vapply(results, nrow, integer(1)))
  change(sprintf(paste(
    "%s 行明细按月 / 商品 / 国家 / 顾客聚合成 %d 张小表 (共 %d 行); 加上 cleaning_audit,",
    "就是 output/ 里提交的 %d 个 CSV (这里写在 trace 目录)"
  ), fmt_int(nrow(cleaned$lines)), length(results), total, length(results) + 1))
  record("queries", "run_queries() + sql/queries.sql", "retail_lines",
         paste(sprintf("%s %d", names(results), vapply(results, nrow, integer(1))), collapse = ", "),
         sprintf("%s -> %d", fmt_int(nrow(cleaned$lines)), total), "按月, 商品, 国家, 顾客聚合")
  results
}

step_analysis <- function(lines) {
  header(5, "report analysis", "daily_revenue() 等", "报告里用 R 算的部分: 日, 星期, 周, 顾客, 预测")
  daily <- timed(daily_revenue, lines)
  log_frame("daily_revenue(): 每个交易日一行, 加 7 天居中滚动均值", daily)
  wd <- timed(weekday_profile, lines)
  log_frame("weekday_profile(): 每个星期几一行 (这家店周六不营业)", wd, sample = nrow(wd))
  weekly <- timed(weekly_revenue, lines)
  log_frame("weekly_revenue(): 连续的周一到周日网格", weekly)
  closed <- weekly$week[weekly$trading_days == 0]
  log_line(sprintf(
    "  %d 周里: 停业 (没有交易日) %d 周 %s; 被数据窗口截断 %d 周",
    nrow(weekly), length(closed), if (length(closed)) paste0("(", paste(closed, collapse = ", "), ")") else "",
    sum(weekly$window_partial)
  ))
  pareto <- timed(customer_pareto, lines)
  log_frame("customer_pareto(): 有 ID 的顾客按收入排序, 累计占比", pareto)
  # The expression the report uses for its "top decile" sentence.
  top10 <- pareto |> filter(customer_pct >= 0.10) |> slice(1) |> pull(revenue_share)
  log_line(sprintf("  收入最高的 10%% 顾客贡献 %.1f%% 的 (有 ID 顾客的) 收入", 100 * top10))
  fc <- timed(forecast_weekly, weekly)
  log_line(sprintf(
    "  forecast_weekly(): 拟合用 %d 周 (%s 到 %s, 最后一次停业之后、去掉截断周), 模型 %s",
    nrow(fc$fit_run), min(fc$fit_run$week), max(fc$fit_run$week), fc$fit$method
  ))
  log_frame("预测 (均值和 80% / 95% 区间)", fc$forecast, sample = nrow(fc$forecast))
  change(sprintf(paste(
    "明细不再往下传, 每个函数各自聚合: 交易日 %d 行, 星期 %d 行, 周 %d 行 (含停业周和截断周,",
    "并标出来), 顾客 %s 行 (累计占比); 预测只用连续的 %d 个完整周拟合 ETS, 输出未来 %d 周"
  ), nrow(daily), nrow(wd), nrow(weekly), fmt_int(nrow(pareto)), nrow(fc$fit_run), nrow(fc$forecast)))
  record("report analysis", "daily_revenue() 等", sprintf("lines %s 行", fmt_int(nrow(lines))),
         sprintf("日 %d, 星期 %d, 周 %d, 顾客 %s, 预测 %d", nrow(daily), nrow(wd), nrow(weekly),
                 fmt_int(nrow(pareto)), nrow(fc$forecast)),
         "聚合", sprintf("ETS 拟合 %d 周 (%s)", nrow(fc$fit_run), fc$fit$method))
}

step_not_run <- function() {
  log_line("")
  log_line(strrep("=", 78))
  log_line("没有运行的部分 (不产生新的数字)")
  log_line(strrep("=", 78))
  log_line("  run_analysis.R 第 4 步: rmarkdown::render() 把 analysis.Rmd 织成 analysis.md 和")
  log_line("      analysis_files/*.svg; 正文和图里的数字来自上面第 2, 4, 5 步的表和函数")
  log_line("  run_analysis.R 的 SVG 字体替换和折行, 以及 output/session_info.txt")
  log_line("  R/verify_outputs.R: 和下面的复现检查做的是同一件事")
}

# ---------------------------------------------------------------- checks

compare_with_committed <- function(out) {
  log_line("")
  log_line(strrep("=", 78))
  log_line("复现检查: trace 写的表 vs 已提交的 output/ (HEAD)")
  log_line(strrep("=", 78))
  for (f in sort(list.files(out, pattern = "\\.csv$"))) {
    new <- readLines(file.path(out, f))
    old <- git("show", paste0("HEAD:output/", f))
    if (is.null(old)) {
      log_line(sprintf("  output/%s: HEAD 里没有这个文件", f))
      next
    }
    if (identical(new, old)) {
      log_line(sprintf("  output/%s: 完全相同 (%d 行)", f, length(new) - 1))
      next
    }
    # The same comparison as R/verify_outputs.R: CSV keeps neither R's
    # integer/double distinction nor Date classes, so compare values.
    a <- read_csv(I(paste(new, collapse = "\n")), show_col_types = FALSE)
    b <- read_csv(I(paste(old, collapse = "\n")), show_col_types = FALSE)
    differ <- if (!identical(names(a), names(b)) || nrow(a) != nrow(b)) "列名或行数" else
      Filter(function(col) {
        if (is.numeric(a[[col]]) && is.numeric(b[[col]])) {
          !isTRUE(all.equal(as.numeric(a[[col]]), as.numeric(b[[col]]), tolerance = 1e-9))
        } else {
          !identical(as.character(a[[col]]), as.character(b[[col]]))
        }
      }, names(a))
    log_line(sprintf(
      "  output/%s: %s", f,
      if (length(differ)) paste("不一致:", paste(differ, collapse = ", ")) else "数值一致 (文本有差别)"
    ))
  }
}

write_lineage <- function(out) {
  lines <- c(
    "# Data lineage (trace run)", "",
    "| 步骤 | 函数 | 输入 | 输出 | 行数变化 | 结构上发生了什么 |",
    "|---|---|---|---|---|---|",
    vapply(lineage, function(r) sprintf(
      "| %s | `%s` | %s | %s | %s | %s |", r$step, r$fn, r$input, r$output, r$rows, r$change
    ), "")
  )
  con <- file(file.path(out, "data_lineage.md"), open = "w", encoding = "UTF-8")
  writeLines(lines, con)
  close(con)
}

main <- function() {
  run_id <- paste0(format(Sys.time(), "%Y%m%dT%H%M%SZ", tz = "UTC"), "_", git_tag())
  out <- proj_path("trace", run_id)
  dir.create(out, recursive = TRUE)
  log_con <<- file(file.path(out, "trace.log"), open = "w", encoding = "UTF-8")
  on.exit(close(log_con))
  options(width = 250)
  log_line("trace run ", run_id, "; 所有输出写在 ", out)

  t0 <- proc.time()[["elapsed"]]
  step_config()
  raw <- step_load()
  cleaned <- step_clean(raw)
  con <- step_sqlite(cleaned$lines, out)
  step_queries(con, cleaned, out)
  step_analysis(cleaned$lines)
  step_not_run()
  write_lineage(out)
  compare_with_committed(out)
  log_line("")
  log_line(sprintf("完成, 共 %.0f s; 日志 %s", proc.time()[["elapsed"]] - t0, file.path(out, "trace.log")))
}

main()
