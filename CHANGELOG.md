# Changelog

- 2026-09-09: Exact duplicate invoice lines are removed by a new first cleaning
  rule, keyed on the same tuple `retail-ai-pipeline` quarantines. The two
  projects now agree to the penny: gross £10,247,353.28, matched credit value
  £385,958.88, net of matched cancellations £9,861,394.40. Previously this
  analysis kept 5,223 duplicated product sale lines worth £24,765.59, and the
  gap between the two headline figures was exactly those rows; the headline
  moves from £9,883,659.86 to £9,861,394.40. Clean sales lines: 525,049 ->
  519,844.

- 2026-09-05: Chronological credit matching restores 263 sale lines and £6,299.05; cancellation-netted revenue is £9,883,659.86.
