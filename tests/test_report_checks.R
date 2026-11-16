# Tests for the checks the report runs on its own conclusions: the
# year-on-year overlap, repeat buying by cohort, and the forecast backtest.
# Hand-built lines, so every expected answer can be checked by eye.

sale_lines <- function(dates, customers, revenue) {
  tibble(
    invoice_ts  = ymd_hm(paste(dates, "09:00")),
    customer_id = customers,
    revenue     = revenue
  )
}

test_yoy_overlap_compares_the_same_calendar_days <- function() {
  # A window from 1 Dec 2010 to 3 Dec 2011 overlaps itself on 1-3 December.
  # The mid-year sale belongs to neither side.
  d <- sale_lines(
    c("2010-12-01", "2010-12-02", "2011-06-15", "2011-12-01", "2011-12-03"),
    rep("1", 5),
    c(100, 50, 999, 120, 60)
  )
  y <- yoy_overlap(d)
  stopifnot(nrow(y) == 2)
  stopifnot(y$from[1] == as_date("2010-12-01"), y$to[1] == as_date("2010-12-03"))
  stopifnot(y$from[2] == as_date("2011-12-01"), y$to[2] == as_date("2011-12-03"))
  stopifnot(all.equal(y$revenue, c(150, 180)))
  stopifnot(all(y$trading_days == 2))
}

test_repeat_by_cohort_groups_by_first_month <- function() {
  # A: first seen in January, back in March -> repeat, January cohort.
  # B: January only, twice on one day -> one visit, not a repeat.
  # C: first seen in March -> March cohort, one-off.
  # No customer id -> in no cohort.
  d <- sale_lines(
    c("2011-01-05", "2011-03-10", "2011-01-06", "2011-01-06", "2011-03-11", "2011-03-12"),
    c("A", "A", "B", "B", "C", NA),
    rep(10, 6)
  )
  co <- repeat_by_cohort(d)
  stopifnot(identical(co$first_month, c("2011-01", "2011-03")))
  stopifnot(identical(co$customers, c(2L, 1L)))
  stopifnot(all.equal(co$repeat_share, c(0.5, 0)))
}

weekly_fixture <- function(revenue) {
  tibble(
    week = as_date("2011-01-03") + days(7 * (seq_along(revenue) - 1)),
    revenue = revenue,
    trading_days = 5L,
    window_partial = FALSE
  )
}

test_backtest_scores_baselines_exactly_on_a_flat_series <- function() {
  # A constant series: both baselines forecast it perfectly.
  bt <- backtest_weekly(weekly_fixture(rep(100, 34)), h = 4, min_train = 26)
  stopifnot(nrow(bt) == 3, all(bt$origins == 5))
  stopifnot(bt$mae[bt$method == "Last week carried forward"] == 0)
  stopifnot(bt$mae[bt$method == "Mean of the last four weeks"] == 0)
}

test_backtest_baseline_error_on_a_step <- function() {
  # 30 weeks at 100, then 4 at 200, one origin (week 30): carrying 100
  # forward misses every one of the next four weeks by 100.
  bt <- backtest_weekly(weekly_fixture(c(rep(100, 30), rep(200, 4))),
                        h = 4, min_train = 30)
  stopifnot(all(bt$origins == 1))
  stopifnot(bt$mae[bt$method == "Last week carried forward"] == 100)
  stopifnot(bt$mae[bt$method == "Mean of the last four weeks"] == 100)
}

test_last_year_weeks_shifts_by_364_days_and_scales <- function() {
  w <- weekly_fixture(c(10, 20, 30))
  out <- last_year_weeks(w, w$week[2:3] + days(364), scale = 1.5)
  stopifnot(identical(out$week, w$week[2:3] + days(364)))
  stopifnot(all.equal(out$revenue, c(30, 45)))
  # A week with no counterpart a year earlier is an error, not an NA.
  failed <- tryCatch({
    last_year_weeks(w, w$week[1], scale = 1)
    FALSE
  }, error = function(e) TRUE)
  stopifnot(failed)
}
