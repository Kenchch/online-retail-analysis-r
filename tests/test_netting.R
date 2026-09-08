# The three revenue measures, on the same hand-built fixture the cleaning tests
# use, where every expected answer can be worked out by eye.
#
# They exist as one function returning one list precisely so that they cannot
# disagree: the README and the report both read these fields, and three numbers
# derived in three places is three chances for one of them to go stale.

# Rows 1-2 are HEART T-LIGHT 6 @ 2.55 for customer 17850; row 3 credits one of
# them. Row 4 credits a CAKESTAND that customer 99999 never bought here. Row 5
# is postage, row 6 a negative-quantity adjustment, row 7 a GBP 20 gift
# voucher, row 8 a zero-price line and row 9 a plain sale.
#
# Gross, under the sales-side rules (no service codes, quantity > 0, price > 0):
#   row 1  6 x 2.55 = 15.30
#   row 2  6 x 2.55 = 15.30
#   row 7  1 x 20   = 20.00   <- gift_0001_20 is NOT in SERVICE_CODES
#   row 9  2 x 3.00 =  6.00
#                     -----
#                     56.60
# Matched: row 3 pairs with row 2 (the latest sale at or before it), removing
#   15.30 and leaving 41.30.
# All product credits: row 3 (15.30) plus row 4 (1 x 12.75) = 28.05, leaving
#   28.55. Row 4 nets nothing under matching because the sale it reverses is
#   not in this file -- which is exactly the gap between the two measures, and
#   why the floor is a floor.

test_netting_measures_are_one_arithmetic <- function() {
  n <- clean_retail(fixture())$netting

  stopifnot(abs(n$gross_value - 56.60) < 1e-9)
  stopifnot(abs(n$net_of_matched_credits - 41.30) < 1e-9)
  stopifnot(abs(n$all_product_credit_value - 28.05) < 1e-9)
  stopifnot(abs(n$net_of_all_product_credits - 28.55) < 1e-9)

  # The three are one subtraction, not three estimates.
  stopifnot(abs(n$gross_value - n$matched_value - n$net_of_matched_credits) < 1e-9)
  stopifnot(
    abs(n$gross_value - n$all_product_credit_value - n$net_of_all_product_credits) < 1e-9
  )
}

test_the_floor_is_never_above_the_headline <- function() {
  # Every matched credit is also a product credit, so subtracting all of them
  # cannot leave more revenue than subtracting some of them.
  #
  # With a tolerance, and the tolerance is the point: when every credit is
  # matched the two measures are equal in exact arithmetic and differ in the
  # last bit, because they are reached by different summation orders. This
  # started as a bare inequality asserted inside clean_retail() and turned the
  # two-credits-three-sales fixture in test_functions.R into a hard failure.
  for (case in list(fixture(), all_credits_matched())) {
    n <- clean_retail(case)$netting
    stopifnot(n$net_of_all_product_credits <= n$net_of_matched_credits + 1e-9)
  }
}

# Three identical sales, two credits: every credit finds a match, so the floor
# and the headline describe the same rows and land on the same value.
all_credits_matched <- function() {
  tibble(
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
}

test_service_credits_stay_out_of_both_measures <- function() {
  # Postage is not product revenue on the sales side, so a postage credit is
  # not product revenue on the credit side either. Netting one against product
  # sales would compare two different things.
  raw <- fixture()
  extra <- raw[raw$stock_code == "POST", ]
  extra$invoice_no <- "C536999"
  extra$quantity <- -1
  with_credit <- rbind(raw, extra)

  before <- clean_retail(raw)$netting
  after <- clean_retail(with_credit)$netting

  stopifnot(after$credit_lines_total == before$credit_lines_total + 1)
  stopifnot(after$credit_lines_service == before$credit_lines_service + 1)
  stopifnot(
    abs(after$all_product_credit_value - before$all_product_credit_value) < 1e-9
  )
  stopifnot(
    abs(after$net_of_all_product_credits - before$net_of_all_product_credits) < 1e-9
  )
}

test_matched_share_uses_every_credit_line_as_its_denominator <- function() {
  # The report says "of all credit-note lines", including the ineligible ones,
  # and calls that process coverage rather than value recovered. The field has
  # to mean the same thing.
  n <- clean_retail(fixture())$netting
  stopifnot(
    abs(n$matched_share_of_credit_lines -
          n$matched_lines / n$credit_lines_total) < 1e-12
  )
  stopifnot(n$credit_lines_eligible <= n$credit_lines_total)
}
