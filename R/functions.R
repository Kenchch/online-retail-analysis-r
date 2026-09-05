# Shared functions for the analysis.
#
# Everything the pipeline and the report both need lives here, so the two can
# never drift apart: run_analysis.R and analysis.Rmd source this file and call
# the same code.

suppressPackageStartupMessages({
  library(readr)
  library(dplyr)
  library(lubridate)
  library(ggplot2)
  library(scales)
  library(DBI)
  library(RSQLite)
  library(zoo)
  library(forecast)
})

# Headless runners often start in the plain "C" locale, where the graphics
# device draws each byte of a multi-byte character separately - every "£" in
# a chart becomes two dots. Switch LC_CTYPE to a UTF-8 locale if one exists.
if (!grepl("utf-?8", Sys.getlocale("LC_CTYPE"), ignore.case = TRUE)) {
  for (loc in c("C.UTF-8", "C.utf8", "en_US.UTF-8", "en_NZ.UTF-8")) {
    suppressWarnings(Sys.setlocale("LC_CTYPE", loc))
    if (grepl("utf-?8", Sys.getlocale("LC_CTYPE"), ignore.case = TRUE)) break
  }
}

# ---------------------------------------------------------------------------
# Paths, resolved against the project root so every script agrees on them.
# ---------------------------------------------------------------------------

proj_root <- function() {
  if (file.exists(file.path("R", "functions.R"))) {
    normalizePath(".")
  } else {
    stop("Run from the project root (the directory containing R/functions.R).")
  }
}

proj_path <- function(...) file.path(proj_root(), ...)
raw_csv_path <- function() proj_path("data", "raw", "online_retail.csv")

# ---------------------------------------------------------------------------
# Load
# ---------------------------------------------------------------------------

# The mirror serves the file as Windows-1252 (a handful of product descriptions
# carry accented characters); reading it as UTF-8 silently mangles them.
read_retail_raw <- function(path = raw_csv_path()) {
  if (!file.exists(path)) {
    stop(
      "Raw data not found at ", path, ".\n",
      "Fetch it first: Rscript R/get_data.R"
    )
  }
  read_csv(
    path,
    locale = locale(encoding = "Windows-1252"),
    col_types = cols(
      InvoiceNo   = col_character(),
      StockCode   = col_character(),
      Description = col_character(),
      Quantity    = col_double(),
      InvoiceDate = col_character(),
      UnitPrice   = col_double(),
      CustomerID  = col_character(),
      Country     = col_character()
    )
  ) |>
    rename(
      invoice_no  = InvoiceNo,
      stock_code  = StockCode,
      description = Description,
      quantity    = Quantity,
      invoice_date = InvoiceDate,
      unit_price  = UnitPrice,
      customer_id = CustomerID,
      country     = Country
    )
}

# ---------------------------------------------------------------------------
# Clean
# ---------------------------------------------------------------------------

# Charges, fees and adjustments that share the invoice-line table with real
# product sales. Kept as an explicit, documented list rather than a pattern:
# codes like DCGSSGIRL or gift_0001_20 look "non-standard" but are genuine
# products, and a regex would throw them away.
SERVICE_CODES <- c(
  "POST", "DOT", "C2",            # postage and carriage
  "M", "S", "D", "B",             # manual entries, samples, discounts, bad-debt adjustments
  "BANK CHARGES", "AMAZONFEE",    # fees
  "CRUK"                          # charity donations
)

#' Apply the cleaning rules in a fixed order and account for every dropped row.
#'
#' Returns list(lines = cleaned tibble, audit = one row per rule with the
#' number of rows it removed). Rules apply sequentially, so each count is
#' "rows removed by this rule that survived the rules above it" and the audit
#' reconciles exactly: rows_in - sum(dropped) == rows_out.
#'
#' The credit-note netting in rule 2 exists because this dataset's most
#' spectacular lines are phantoms: the two largest orders of the year (80,995
#' and 74,215 units) were both cancelled minutes after being keyed in. Simply
#' dropping the credit notes would leave those "sales" in every headline
#' number; matching each credit back to a sale removes both sides of the pair.
clean_retail <- function(raw) {
  audit <- list()
  step <- function(d, rule, keep) {
    dropped <- sum(!keep)
    audit[[length(audit) + 1]] <<- tibble(rule = rule, rows_dropped = dropped)
    d[keep, ]
  }

  d <- raw |>
    mutate(invoice_ts = mdy_hm(invoice_date)) |>
    select(-invoice_date)
  stopifnot(!anyNA(d$invoice_ts))
  n_in <- nrow(d)

  # Rule 1: pull the credit notes out (kept aside for rule 2's matching).
  is_credit <- grepl("^C", d$invoice_no)
  credits <- d[is_credit, ]
  d <- step(d, "Credit notes (InvoiceNo starting with 'C')", !is_credit)

  # Rule 2: net out the sales those credit notes cancel. A credit is matched
  # 1:1 to a sale line with the same customer, product, quantity and price;
  # process credits in timestamp order and take the latest unused sale at or
  # before each credit. Equal timestamps are eligible because source times
  # only have minute precision; input row order breaks ties. Guest lines are never
  # matched - the pairing would be guesswork. Credits with no matching sale
  # (returns of pre-window sales, partial returns) net nothing: they are
  # already dropped, so revenue stays slightly gross of returns.
  d <- d |> mutate(.row = row_number())
  eligible_credits <- credits |>
    mutate(.credit_row = row_number()) |>
    filter(!is.na(customer_id), quantity < 0) |>
    transmute(customer_id, stock_code, quantity = -quantity, unit_price,
              .credit_row, .credit_ts = invoice_ts)
  candidates <- d |>
    filter(!is.na(customer_id)) |>
    inner_join(eligible_credits,
               by = c("customer_id", "stock_code", "quantity", "unit_price"),
               relationship = "many-to-many") |>
    filter(invoice_ts <= .credit_ts) |>
    arrange(.credit_ts, .credit_row, desc(invoice_ts), desc(.row))
  used_sales <- rep(FALSE, nrow(d))
  used_credits <- rep(FALSE, nrow(credits))
  for (i in seq_len(nrow(candidates))) {
    sale <- candidates$.row[i]
    credit <- candidates$.credit_row[i]
    if (!used_sales[sale] && !used_credits[credit]) {
      used_sales[sale] <- TRUE
      used_credits[credit] <- TRUE
    }
  }
  offset_rows <- which(used_sales)
  d <- step(d, "Sales offset by a matching credit note (same customer, product, quantity, price)",
            !(d$.row %in% offset_rows))
  d$.row <- NULL

  d <- step(d, "Service charges / adjustments (postage, fees, manual entries)",
            !(toupper(d$stock_code) %in% SERVICE_CODES))
  d <- step(d, "Non-positive quantity (stock corrections without a credit note)",
            d$quantity > 0)
  d <- step(d, "Non-positive unit price (damaged / unsaleable write-offs)",
            d$unit_price > 0)

  d <- d |> mutate(revenue = quantity * unit_price)

  audit_tbl <- bind_rows(audit) |>
    mutate(share_of_input = rows_dropped / n_in)

  stopifnot(n_in - sum(audit_tbl$rows_dropped) == nrow(d))

  list(lines = d, audit = audit_tbl, rows_in = n_in, rows_out = nrow(d))
}

file_sha256 <- function(path) {
  digest::digest(file = path, algo = "sha256", serialize = FALSE)
}

# Cache the cleaned table so the report can be re-knitted without re-reading
# the 45 MB raw file. The cache carries a fingerprint of the raw file AND of
# this source file, so editing a cleaning rule or swapping the data busts it
# automatically - a cache with no invalidation would silently rebuild every
# committed artefact from stale data. The cache is derived and stays out of git.
cleaned_lines <- function() {
  cache <- proj_path("cache", "clean_retail.rds")
  fingerprint <- list(
    raw  = file_sha256(raw_csv_path()),
    code = file_sha256(proj_path("R", "functions.R"))
  )
  if (file.exists(cache)) {
    hit <- readRDS(cache)
    if (identical(hit$fingerprint, fingerprint)) return(hit$cleaned)
  }
  cleaned <- clean_retail(read_retail_raw())
  dir.create(dirname(cache), showWarnings = FALSE, recursive = TRUE)
  saveRDS(list(fingerprint = fingerprint, cleaned = cleaned), cache)
  cleaned
}

# ---------------------------------------------------------------------------
# SQL layer
# ---------------------------------------------------------------------------

#' Load the cleaned lines into SQLite. Timestamps are stored as ISO-8601 text
#' so SQLite's strftime() can group by month/day directly.
build_sqlite <- function(lines, db_path = proj_path("cache", "retail.sqlite")) {
  dir.create(dirname(db_path), showWarnings = FALSE, recursive = TRUE)
  con <- dbConnect(SQLite(), db_path)
  dbWriteTable(
    con, "retail_lines",
    lines |> mutate(invoice_ts = format(invoice_ts, "%Y-%m-%d %H:%M:%S")),
    overwrite = TRUE
  )
  con
}

#' Parse sql/queries.sql into a named list. Queries are separated by
#' "-- name: <query_name>" headers.
read_queries <- function(path = proj_path("sql", "queries.sql")) {
  lines <- readLines(path)
  starts <- grep("^-- name:", lines)
  stopifnot(length(starts) > 0)
  ends <- c(starts[-1] - 1, length(lines))
  queries <- Map(function(s, e) paste(lines[(s + 1):e], collapse = "\n"), starts, ends)
  names(queries) <- trimws(sub("^-- name:", "", lines[starts]))
  queries
}

#' Run every named query and return a named list of data frames.
run_queries <- function(con, queries = read_queries()) {
  lapply(queries, function(q) as_tibble(dbGetQuery(con, q)))
}

# ---------------------------------------------------------------------------
# Analysis
# ---------------------------------------------------------------------------

daily_revenue <- function(lines) {
  lines |>
    mutate(day = as_date(invoice_ts)) |>
    summarise(revenue = sum(revenue), .by = day) |>
    arrange(day) |>
    mutate(rolling_7d = rollmean(revenue, k = 7, fill = NA, align = "center"))
}

weekday_profile <- function(lines) {
  lines |>
    mutate(
      day     = as_date(invoice_ts),
      weekday = wday(invoice_ts, label = TRUE, abbr = TRUE, week_start = 1, locale = "C")
    ) |>
    summarise(revenue = sum(revenue), .by = c(weekday, day)) |>
    summarise(
      mean_daily_revenue = mean(revenue),
      trading_days       = n(),
      .by = weekday
    ) |>
    arrange(weekday)
}

#' Weekly revenue on a complete ISO-week (Mon-Sun) grid. Building on a grid
#' matters: the retailer shuts over year-end, and a groupby alone would simply
#' omit the closure week, leaving a silent 14-day hole that downstream code
#' (which assumes consecutive weeks) would never notice. Closure weeks appear
#' here as zero-revenue rows. window_partial flags the weeks the data window
#' truncates at either end: comparing a 3-day week against full weeks
#' manufactures a fake collapse, so trend and forecast code excludes them.
weekly_revenue <- function(lines) {
  span <- range(as_date(lines$invoice_ts))
  observed <- lines |>
    mutate(week = floor_date(as_date(invoice_ts), "week", week_start = 1)) |>
    summarise(
      revenue      = sum(revenue),
      trading_days = n_distinct(as_date(invoice_ts)),
      .by = week
    )
  tibble(week = seq(floor_date(span[1], "week", week_start = 1),
                    floor_date(span[2], "week", week_start = 1),
                    by = "7 days")) |>
    left_join(observed, by = "week") |>
    mutate(
      revenue        = coalesce(revenue, 0),
      trading_days   = coalesce(trading_days, 0L),
      window_partial = week < span[1] | week + days(6) > span[2]
    )
}

#' Cumulative revenue share by customer rank (identified customers only).
customer_pareto <- function(lines) {
  lines |>
    filter(!is.na(customer_id)) |>
    summarise(revenue = sum(revenue), .by = customer_id) |>
    arrange(desc(revenue)) |>
    mutate(
      customer_pct = row_number() / n(),
      revenue_share = cumsum(revenue) / sum(revenue)
    )
}

#' Four-week-ahead ETS forecast. The model is fit on the unbroken run of
#' trading weeks AFTER the last year-end closure week: ts() assumes equally
#' spaced, comparable observations, and feeding it the closure (a near-zero
#' week) or the window-truncated edge weeks would hand the level estimate a
#' collapse that never happened. Deliberately non-seasonal too: with 13 months
#' of history there is at most one observation of any annual effect, so a
#' seasonal term would be fit to noise. The report says the same thing in
#' prose - this is an illustration of method, not a number to bank.
forecast_weekly <- function(weekly, h = 4) {
  usable <- weekly |> filter(!window_partial)
  closures <- usable$week[usable$trading_days <= 1]
  fit_run <- usable |>
    filter(if (length(closures)) week > max(closures) else TRUE)
  stopifnot(all(diff(as.integer(fit_run$week)) == 7))
  fit <- ets(ts(fit_run$revenue), model = "ZZN")
  fc <- forecast(fit, h = h)
  list(
    fit = fit,
    forecast = tibble(
      week  = max(fit_run$week) + weeks(seq_len(h)),
      mean  = as.numeric(fc$mean),
      lo80  = as.numeric(fc$lower[, 1]),
      hi80  = as.numeric(fc$upper[, 1]),
      lo95  = as.numeric(fc$lower[, 2]),
      hi95  = as.numeric(fc$upper[, 2])
    ),
    history = usable,
    fit_run = fit_run
  )
}

# ---------------------------------------------------------------------------
# Visual identity - one palette, applied everywhere.
# ---------------------------------------------------------------------------

PAL <- list(
  blue    = "#2a78d6",  # primary series
  orange  = "#eb6834",  # second series (forecast)
  surface = "#fcfcfb",
  ink     = "#0b0b0b",
  ink2    = "#52514e",
  muted   = "#898781",
  grid    = "#e1e0d9",
  band    = "#cde2fb"   # light step of the blue ramp, for intervals
)

theme_retail <- function() {
  # base_family matters for SVG output: with no family, svglite writes an
  # empty font-family and browsers fall back to a serif. "Helvetica" maps
  # cleanly on every platform (Arial on Windows, Liberation Sans on Linux).
  theme_minimal(base_size = 12, base_family = "Helvetica") +
    theme(
      plot.background   = element_rect(fill = PAL$surface, colour = NA),
      panel.background  = element_rect(fill = PAL$surface, colour = NA),
      panel.grid.minor  = element_blank(),
      panel.grid.major  = element_line(colour = PAL$grid, linewidth = 0.3),
      text              = element_text(colour = PAL$ink),
      axis.text         = element_text(colour = PAL$muted),
      axis.title        = element_text(colour = PAL$ink2, size = 10),
      plot.title        = element_text(face = "bold", size = 13),
      plot.subtitle     = element_text(colour = PAL$ink2, size = 10),
      plot.caption      = element_text(colour = PAL$muted, size = 8, hjust = 0),
      legend.text       = element_text(colour = PAL$ink2)
    )
}

# Hand-rolled rather than label_currency(scale_cut = cut_short_scale()):
# scales 1.3.0's scale_cut errors on some value ranges ("NAs are not allowed
# in subscripted assignments"), and axis formatting is not worth a fragile
# dependency. NA-safe because ggplot passes NA breaks.
label_gbp <- function(x) {
  vapply(x, function(v) {
    if (is.na(v)) return(NA_character_)
    a <- abs(v)
    if (a >= 1e6) {
      paste0("£", format(round(v / 1e6, 2), trim = TRUE, drop0trailing = TRUE), "m")
    } else if (a >= 1e3) {
      paste0("£", format(round(v / 1e3), trim = TRUE, big.mark = ","), "k")
    } else {
      paste0("£", format(round(v), trim = TRUE, big.mark = ","))
    }
  }, character(1))
}
