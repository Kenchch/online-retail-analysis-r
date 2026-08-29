# Unit tests for the cleaning and SQL plumbing, on a hand-built fixture where
# every expected answer can be checked by eye.
#
#     Rscript tests/run_tests.R
#
# Plain stopifnot() rather than a test framework: the suite is small, and the
# analysis should be runnable with nothing beyond the packages the analysis
# itself uses.

fixture <- function() {
  tibble(
    invoice_no  = c("536365", "536366", "C536379", "C536380",
                    "536381", "536382", "536383", "536384", "536385"),
    stock_code  = c("85123A", "85123A", "85123A", "22423",
                    "POST",   "21212",  "gift_0001_20", "22423", "21730"),
    description = c("HEART T-LIGHT", "HEART T-LIGHT", "HEART T-LIGHT", "CAKESTAND",
                    "POSTAGE", "ADJUSTMENT", "GIFT VOUCHER", "CAKESTAND", "JAR"),
    quantity    = c(6, 6, -6, -1, 1, -20, 1, 3, 2),
    invoice_date = c("12/1/2010 8:26", "12/1/2010 9:00", "12/1/2010 9:10", "12/1/2010 9:20",
                     "12/1/2010 8:26", "12/1/2010 8:26", "12/1/2010 8:26",
                     "12/1/2010 8:26", "12/1/2010 8:26"),
    unit_price  = c(2.55, 2.55, 2.55, 12.75, 18, 1.00, 20, 0, 3.00),
    customer_id = c("17850", "17850", "17850", "99999",
                    "17850", NA, "13047", "13047", NA),
    country     = rep("United Kingdom", 9)
  )
}

# The fixture, rule by rule:
#   row 3 (C536379)  credit note matching rows 1-2               -> rule 1
#   row 4 (C536380)  credit note with no matching sale           -> rule 1
#   one of rows 1-2  the sale that credit note cancels (1:1)     -> rule 2
#   row 5 (POST)     service charge                              -> rule 3
#   row 6 (qty -20)  stock correction without a credit note      -> rule 4
#   row 8 (price 0)  unsaleable write-off                        -> rule 5
# Survivors: one of the twin 85123A sales, the gift voucher, the guest sale.

test_cleaning_rules <- function() {
  res <- clean_retail(fixture())

  stopifnot(res$rows_in == 9, res$rows_out == 3, nrow(res$lines) == 3)
  stopifnot(all(res$audit$rows_dropped == c(2, 1, 1, 1, 1)))

  # The credit was for ONE sale of 6 units; only one of the two identical
  # sales may be netted out - the later one, matching how cancellations
  # trail their sales.
  survivors_85123A <- res$lines[res$lines$stock_code == "85123A", ]
  stopifnot(nrow(survivors_85123A) == 1)
  stopifnot(format(survivors_85123A$invoice_ts, "%H:%M") == "08:26")

  # The gift voucher is a product, not a service charge - it must survive.
  stopifnot("gift_0001_20" %in% res$lines$stock_code)

  # Guest lines (no customer id) are never matched to credits.
  stopifnot("21730" %in% res$lines$stock_code)

  # The audit reconciles: in - dropped == out.
  stopifnot(res$rows_in - sum(res$audit$rows_dropped) == res$rows_out)

  # Revenue and timestamps are derived correctly.
  stopifnot(survivors_85123A$revenue == 6 * 2.55)
}

test_rule_order_reported_sequentially <- function() {
  # A credit note written against a service code must be counted once, by the
  # first rule that catches it, or the audit double-counts and stops
  # reconciling.
  d <- fixture()
  d$invoice_no[5] <- "C536381"  # the POST line is now also a credit note
  res <- clean_retail(d)
  stopifnot(res$audit$rows_dropped[1] == 3)  # all three credits counted here
  stopifnot(res$audit$rows_dropped[3] == 0)  # not counted again as a service line
  stopifnot(res$rows_in - sum(res$audit$rows_dropped) == res$rows_out)
}

test_netting_is_one_to_one <- function() {
  # Two credits against three identical sales must remove exactly two sales,
  # and the earliest sale is the one that survives.
  d <- tibble(
    invoice_no  = c("1", "2", "3", "C4", "C5"),
    stock_code  = rep("85123A", 5),
    description = rep("HEART T-LIGHT", 5),
    quantity    = c(6, 6, 6, -6, -6),
    invoice_date = c("12/1/2010 8:00", "12/1/2010 9:00", "12/1/2010 10:00",
                     "12/1/2010 10:30", "12/1/2010 10:40"),
    unit_price  = rep(2.55, 5),
    customer_id = rep("17850", 5),
    country     = rep("United Kingdom", 5)
  )
  res <- clean_retail(d)
  stopifnot(nrow(res$lines) == 1)
  stopifnot(format(res$lines$invoice_ts, "%H:%M") == "08:00")
  stopifnot(res$audit$rows_dropped[1] == 2, res$audit$rows_dropped[2] == 2)
}

test_sql_roundtrip <- function() {
  res <- clean_retail(fixture())
  con <- build_sqlite(res$lines, db_path = file.path(tempdir(), "test_retail.sqlite"))
  on.exit(dbDisconnect(con))
  out <- run_queries(con)

  stopifnot(setequal(
    names(out),
    c("monthly_summary", "monthly_growth", "top_products", "country_summary", "customer_repeat")
  ))

  # One month of data, and SQL's revenue total must equal R's.
  stopifnot(nrow(out$monthly_summary) == 1)
  stopifnot(abs(out$monthly_summary$revenue - sum(res$lines$revenue)) < 1e-6)

  # Guest-checkout revenue (missing customer_id) is excluded from the
  # customer split, so its revenue must total less than the overall figure.
  stopifnot(sum(out$customer_repeat$revenue) < sum(res$lines$revenue))

  # The monthly growth table carries the trading-day count that flags
  # partial months in the standalone CSV.
  stopifnot("trading_days" %in% names(out$monthly_growth))
}

test_top_products_group_case_insensitively <- function() {
  # '85123a' and '85123A' are the same product; a case-sensitive GROUP BY
  # would split it into two rows.
  d <- fixture()
  d$stock_code[2] <- "85123a"
  d$invoice_no[3] <- "536379"; d$quantity[3] <- 6  # neutralise the credit note
  res <- clean_retail(d)
  con <- build_sqlite(res$lines, db_path = file.path(tempdir(), "test_retail2.sqlite"))
  on.exit(dbDisconnect(con))
  top <- run_queries(con)$top_products
  stopifnot(sum(top$stock_code == "85123A") == 1)
  stopifnot(top$units[top$stock_code == "85123A"] == 18)
}

test_weekly_grid_has_no_holes <- function() {
  # Two sales five weeks apart: the weeks between them must exist as
  # zero-revenue closure rows, not be silently absent.
  d <- tibble(
    invoice_no  = c("1", "2"),
    stock_code  = rep("85123A", 2),
    description = rep("HEART T-LIGHT", 2),
    quantity    = c(6, 6),
    invoice_date = c("1/3/2011 8:00", "2/7/2011 8:00"),
    unit_price  = rep(2.55, 2),
    customer_id = rep("17850", 2),
    country     = rep("United Kingdom", 2)
  )
  weekly <- weekly_revenue(clean_retail(d)$lines)
  stopifnot(nrow(weekly) == 6)
  stopifnot(all(diff(as.integer(weekly$week)) == 7))
  stopifnot(sum(weekly$revenue == 0) == 4)
}
