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
    # UTC with the date, like the run id and the other projects' traces.
    out <- paste0(format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"), "  ", line)
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
  log_line(sprintf("  %s: %s rows x %d columns, %.1f MB in memory",
                   label, fmt_int(nrow(df)), ncol(df), mem))
  for (name in names(df)) {
    col <- df[[name]]
    info <- sprintf("%-22s NAs %8s", paste(class(col), collapse = "/"), fmt_int(sum(is.na(col))))
    if (is.logical(col)) {
      info <- paste0(info, "  TRUE ", fmt_int(sum(col, na.rm = TRUE)))
    } else if (is.numeric(col) || inherits(col, c("Date", "POSIXct"))) {
      if (any(!is.na(col))) {
        r <- range(col, na.rm = TRUE)
        info <- paste0(info, "  range [", fmt_val(r[1]), ", ", fmt_val(r[2]), "]")
      }
    } else {
      info <- paste0(info, "  ", fmt_int(n_distinct(col, na.rm = TRUE)), " distinct values")
    }
    log_line(sprintf("      %-24s %s", name, info))
  }
  shown <- as.data.frame(utils::head(df[, seq_len(min(max_cols, ncol(df))), drop = FALSE], sample))
  shown[] <- lapply(shown, function(v) {
    if (is.character(v)) ifelse(nchar(v) > 22, paste0(substr(v, 1, 19), "..."), v) else v
  })
  extra <- if (ncol(df) > max_cols) sprintf("  (%d columns not shown)", ncol(df) - max_cols) else ""
  log_line(sprintf("      first %d rows%s:", min(sample, nrow(df)), extra))
  log_line(paste0("      | ", utils::capture.output(print(shown)), collapse = "\n"))
}

#' A small frame in full: the audit, query plans.
log_table <- function(df) {
  log_line(paste0(
    "      | ", utils::capture.output(print(as.data.frame(df), row.names = FALSE)),
    collapse = "\n"
  ))
}

change <- function(text) log_line("  -> what changed: ", text)

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
      log_line("[R] warning: ", conditionMessage(w))
      invokeRestart("muffleWarning")
    }
  )
  log_line(sprintf("  took %.1f s", proc.time()[["elapsed"]] - t0))
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
  header(0, "config", "R/functions.R", "the constants and environment every step shares")
  log_line("  ", R.version.string)
  pkgs <- c("readr", "dplyr", "lubridate", "DBI", "RSQLite", "zoo", "forecast")
  log_line("  package versions (renv.lock): ", paste(
    sprintf("%s %s", pkgs, vapply(pkgs, function(p) as.character(packageVersion(p)), "")),
    collapse = ", "
  ))
  log_line("  raw_csv_path(): ", raw_csv_path())
  log_line("  DUPLICATE_KEY: ", paste(DUPLICATE_KEY, collapse = ", "))
  log_line("  SERVICE_CODES: ", paste(SERVICE_CODES, collapse = ", "))
  log_line("  sql/queries.sql: ", paste(names(read_queries()), collapse = ", "))
}

step_load <- function() {
  header(1, "load", "read_retail_raw()", "read the CSV with a type for every column, rename the columns")
  path <- raw_csv_path()
  pinned <- sub(
    '.*"([0-9a-f]{64})".*', "\\1",
    grep("^EXPECTED_SHA256", readLines(proj_path("R", "get_data.R")), value = TRUE)
  )
  digest <- file_sha256(path)
  log_line(sprintf(
    "  %s: %s bytes, sha256 %s... -> %s",
    basename(path), fmt_int(file.size(path)), substr(digest, 1, 16),
    if (identical(digest, pinned)) "matches the pin in R/get_data.R" else "DOES NOT MATCH the pin in R/get_data.R"
  ))
  raw <- timed(read_retail_raw)
  log_frame("read_retail_raw() output", raw)
  change(paste(
    "no rows dropped. The 8 columns are read by col_types: Quantity / UnitPrice as",
    "double, the rest (InvoiceDate and CustomerID included) as character; names become",
    "lower snake_case. Times are still strings"
  ))
  record("load", "read_retail_raw()", sprintf("%s (%.0f MB)", basename(path), file.size(path) / 2^20),
         sprintf("raw, %s rows x %d columns", fmt_int(nrow(raw)), ncol(raw)), "x1 (no rows dropped)",
         "a type per column; columns renamed")
  raw
}

step_clean <- function(raw) {
  header(2, "clean", "clean_retail()",
         "rules run in turn, each recording the rows it dropped; credit notes net off sales one to one")
  cleaned <- timed(clean_retail, raw)
  audit <- cleaned$audit |>
    mutate(
      rows_left = cleaned$rows_in - cumsum(rows_dropped),
      share_of_input = sprintf("%.2f%%", 100 * share_of_input)
    )
  log_line("  rules in the order they run (each counts only the rows the earlier ones left, so these add up):")
  log_table(audit)
  lines <- cleaned$lines
  log_frame("clean_retail() output: lines", lines)
  dropped <- cleaned$rows_in - cleaned$rows_out
  log_line(sprintf(
    "  check: rows_in %s - dropped %s = %s vs rows_out %s -> %s",
    fmt_int(cleaned$rows_in), fmt_int(sum(cleaned$audit$rows_dropped)),
    fmt_int(cleaned$rows_in - sum(cleaned$audit$rows_dropped)), fmt_int(cleaned$rows_out),
    if (cleaned$rows_in - sum(cleaned$audit$rows_dropped) == cleaned$rows_out) "adds up" else "DOES NOT ADD UP"
  ))
  log_line("  credit-note netting (cleaned$netting):")
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
    "%s rows -> %s rows (%s dropped, %.1f%%). invoice_date string -> invoice_ts (POSIXct,",
    "parsed with mdy_hm); revenue = quantity x unit_price added; credit notes (invoice",
    "numbers starting with C) dropped whole, along with the %s sales lines they net off",
    "one to one"
  ), fmt_int(cleaned$rows_in), fmt_int(cleaned$rows_out), fmt_int(dropped),
  100 * dropped / cleaned$rows_in, fmt_int(cleaned$netting$matched_lines)))
  record("clean", "clean_retail()", sprintf("raw, %s rows", fmt_int(cleaned$rows_in)),
         sprintf("lines, %s rows x %d columns", fmt_int(nrow(lines)), ncol(lines)),
         sprintf("-%s (%.1f%%)", fmt_int(dropped), 100 * dropped / cleaned$rows_in),
         sprintf("%d rules drop rows in turn; credit notes netted; invoice_ts, revenue added",
                 nrow(audit)))
  cleaned
}

step_sqlite <- function(lines, out) {
  header(3, "sqlite", "build_sqlite()", "the clean lines go into SQLite, times stored as ISO text")
  db <- file.path(out, "retail.sqlite")
  con <- timed(build_sqlite, lines, db_path = db)
  n <- dbGetQuery(con, "SELECT count(*) AS n FROM retail_lines")$n
  log_line(sprintf("  retail_lines: %s rows, %s (%.1f MB)", fmt_int(n), db, file.size(db) / 2^20))
  log_line("  column types in SQLite:")
  log_table(dbGetQuery(con, "PRAGMA table_info(retail_lines)")[, c("name", "type")])
  log_line("  first 3 rows:")
  log_table(dbGetQuery(con, "SELECT * FROM retail_lines LIMIT 3"))
  log_line(sprintf("  check: %s rows vs lines %s rows -> %s",
                   fmt_int(n), fmt_int(nrow(lines)), if (n == nrow(lines)) "matches" else "DOES NOT MATCH"))
  change(paste(
    "row count unchanged. POSIXct times become '%Y-%m-%d %H:%M:%S' text (SQLite has no",
    "time type, and strftime() takes the month and day straight from this text); numeric",
    "columns are stored as REAL, character columns as TEXT"
  ))
  record("sqlite", "build_sqlite()", sprintf("lines, %s rows", fmt_int(nrow(lines))),
         sprintf("retail_lines, %s rows", fmt_int(n)), "unchanged", "POSIXct -> ISO text")
  con
}

step_queries <- function(con, cleaned, out) {
  header(4, "queries", "read_queries() + run_queries()",
         "the named queries in sql/queries.sql, one summary table each")
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
    "%s detail lines aggregated by month / product / country / customer into %d small",
    "tables (%d rows in all); with cleaning_audit, these are the %d CSVs committed in",
    "output/ (written to the trace directory here)"
  ), fmt_int(nrow(cleaned$lines)), length(results), total, length(results) + 1))
  record("queries", "run_queries() + sql/queries.sql", "retail_lines",
         paste(sprintf("%s %d", names(results), vapply(results, nrow, integer(1))), collapse = ", "),
         sprintf("%s -> %d", fmt_int(nrow(cleaned$lines)), total),
         "aggregated by month, product, country, customer")
  results
}

step_analysis <- function(lines) {
  header(5, "report analysis", "daily_revenue() and others",
         "what the report computes in R: days, weekdays, weeks, customers, forecast")
  daily <- timed(daily_revenue, lines)
  log_frame("daily_revenue(): one row per trading day, with a centred 7-day rolling mean", daily)
  wd <- timed(weekday_profile, lines)
  log_frame("weekday_profile(): one row per weekday (the shop does not trade on Saturdays)", wd,
            sample = nrow(wd))
  weekly <- timed(weekly_revenue, lines)
  log_frame("weekly_revenue(): an unbroken grid of Monday-to-Sunday weeks", weekly)
  closed <- weekly$week[weekly$trading_days == 0]
  log_line(sprintf(
    "  of %d weeks: %d closed (no trading days) %s; %d cut short by the data window",
    nrow(weekly), length(closed), if (length(closed)) paste0("(", paste(closed, collapse = ", "), ")") else "",
    sum(weekly$window_partial)
  ))
  pareto <- timed(customer_pareto, lines)
  log_frame("customer_pareto(): customers with an ID ranked by revenue, with cumulative shares", pareto)
  # The expression the report uses for its "top decile" sentence.
  top10 <- pareto |> filter(customer_pct >= 0.10) |> slice(1) |> pull(revenue_share)
  log_line(sprintf(
    "  the top 10%% of customers by revenue bring in %.1f%% of the revenue from customers with an ID",
    100 * top10
  ))
  fc <- timed(forecast_weekly, weekly)
  log_line(sprintf(
    "  forecast_weekly(): fitted on %d weeks (%s to %s: after the last closure, no cut-short weeks), model %s",
    nrow(fc$fit_run), min(fc$fit_run$week), max(fc$fit_run$week), fc$fit$method
  ))
  log_frame("forecast (mean and 80% / 95% intervals)", fc$forecast, sample = nrow(fc$forecast))
  change(sprintf(paste(
    "the detail lines go no further; each function aggregates on its own: trading days %d",
    "rows, weekdays %d rows, weeks %d rows (closed and cut-short weeks included, and",
    "flagged), customers %s rows (cumulative shares); the forecast fits ETS on only the %d",
    "consecutive whole weeks and gives the next %d weeks"
  ), nrow(daily), nrow(wd), nrow(weekly), fmt_int(nrow(pareto)), nrow(fc$fit_run), nrow(fc$forecast)))
  record("report analysis", "daily_revenue() and others", sprintf("lines, %s rows", fmt_int(nrow(lines))),
         sprintf("days %d, weekdays %d, weeks %d, customers %s, forecast %d", nrow(daily), nrow(wd),
                 nrow(weekly), fmt_int(nrow(pareto)), nrow(fc$forecast)),
         "aggregated", sprintf("ETS fitted on %d weeks (%s)", nrow(fc$fit_run), fc$fit$method))
}

step_not_run <- function() {
  log_line("")
  log_line(strrep("=", 78))
  log_line("Not run (it produces no new numbers)")
  log_line(strrep("=", 78))
  log_line("  run_analysis.R step 4: rmarkdown::render() knits analysis.Rmd into analysis.md and")
  log_line("      analysis_files/*.svg; the numbers in its text and figures come from the tables")
  log_line("      and functions of steps 2, 4 and 5 above")
  log_line("  run_analysis.R's SVG font swap and line wrapping, and output/session_info.txt")
  log_line("  R/verify_outputs.R: the same check as the reproducibility check below")
}

# ---------------------------------------------------------------- checks

compare_with_committed <- function(out) {
  log_line("")
  log_line(strrep("=", 78))
  log_line("Reproducibility check: the trace's tables vs the committed output/ (HEAD)")
  log_line(strrep("=", 78))
  for (f in sort(list.files(out, pattern = "\\.csv$"))) {
    new <- readLines(file.path(out, f))
    old <- git("show", paste0("HEAD:output/", f))
    if (is.null(old)) {
      log_line(sprintf("  output/%s: not in HEAD", f))
      next
    }
    if (identical(new, old)) {
      log_line(sprintf("  output/%s: identical (%d rows)", f, length(new) - 1))
      next
    }
    # The same comparison as R/verify_outputs.R: CSV keeps neither R's
    # integer/double distinction nor Date classes, so compare values.
    a <- read_csv(I(paste(new, collapse = "\n")), show_col_types = FALSE)
    b <- read_csv(I(paste(old, collapse = "\n")), show_col_types = FALSE)
    differ <- if (!identical(names(a), names(b)) || nrow(a) != nrow(b)) "column names or row count" else
      Filter(function(col) {
        if (is.numeric(a[[col]]) && is.numeric(b[[col]])) {
          !isTRUE(all.equal(as.numeric(a[[col]]), as.numeric(b[[col]]), tolerance = 1e-9))
        } else {
          !identical(as.character(a[[col]]), as.character(b[[col]]))
        }
      }, names(a))
    log_line(sprintf(
      "  output/%s: %s", f,
      if (length(differ)) paste("DIFFERS:", paste(differ, collapse = ", ")) else "values match (the text differs)"
    ))
  }
}

write_lineage <- function(out) {
  lines <- c(
    "# Data lineage (trace run)", "",
    "| Step | Function | Input | Output | Rows | What happened to the structure |",
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
  log_line("trace run ", run_id, "; everything is written under ", out)

  t0 <- proc.time()[["elapsed"]]
  # A failing step stops the script, and stop() only reaches the console. Log it
  # first, so trace.log ends with the error under the header of the step that
  # raised it rather than at the header alone.
  tryCatch(run_steps(out), error = function(e) {
    log_line(sprintf("trace failed after %.0f s", proc.time()[["elapsed"]] - t0))
    log_line("  error: ", conditionMessage(e))
    call <- conditionCall(e)
    if (!is.null(call)) log_line("  call: ", paste(deparse(call), collapse = " "))
    stop(e)
  })
  log_line("")
  log_line(sprintf("done in %.0f s; log %s", proc.time()[["elapsed"]] - t0, file.path(out, "trace.log")))
}

run_steps <- function(out) {
  step_config()
  raw <- step_load()
  cleaned <- step_clean(raw)
  con <- step_sqlite(cleaned$lines, out)
  step_queries(con, cleaned, out)
  step_analysis(cleaned$lines)
  step_not_run()
  write_lineage(out)
  compare_with_committed(out)
}

main()
