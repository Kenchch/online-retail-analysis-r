# Changelog

- 2026-11-16: Five conclusions in the report are corrected or given the
  evidence they lacked. No cleaning rule, table or headline figure changes.
  - Lines with no customer ID are no longer called guest checkouts. Their
    invoices average 96 lines and £1,102, against 21 lines and £457 with an ID.
  - The third revenue measure is the period basis, not a floor, and the claim
    that the true net lies between it and the headline is withdrawn.
  - The 64% repeat rate is shown by first-purchase month, since it mixes
    customers who had a year to return with customers who had a week.
  - Concentration is stated for the 5, 10 and 20 largest accounts instead of
    resting on a decile of 433 customers.
  - The forecast is backtested against two baselines and set against the same
    weeks of 2010. It does not beat a four-week mean, and it overstates the
    four December weeks by a factor of about 1.9.
  - New: a like-for-like year-on-year comparison, 1–9 December, +9.6%.

- 2026-10-24: `R/trace_pipeline.R` runs every step into `trace/<run_id>/` and
  logs how the data changes shape at each one, then compares its tables with
  the committed `output/`. No figure changes.

- 2026-09-09: Exact duplicate invoice lines are removed by a new first cleaning
  rule, keyed on the same tuple `retail-ai-pipeline` quarantines. The two
  projects now agree to the penny: gross £10,247,353.28, matched credit value
  £385,958.88, net of matched cancellations £9,861,394.40. Previously this
  analysis kept 5,223 duplicated product sale lines worth £24,765.59, and the
  gap between the two headline figures was exactly those rows; the headline
  moves from £9,883,659.86 to £9,861,394.40. Clean sales lines: 525,049 ->
  519,844.

- 2026-09-05: Chronological credit matching restores 263 sale lines and £6,299.05; cancellation-netted revenue is £9,883,659.86.
