# The README states the same figures the report does, in its own wording.
# Two copies of a number is two chances to leave one behind, and this pair has
# now moved twice: once for chronological matching, once for the duplicate rule.
#
# These read the committed audit CSV rather than re-running the pipeline, so
# they are cheap and run in the same suite as everything else. run_analysis.R
# writes that CSV, and CI rebuilds it and diffs it, so a stale CSV cannot hide
# a stale README.

readme_text <- function() paste(readLines("README.md", warn = FALSE), collapse = "
")

committed_audit <- function() {
  read_csv(proj_path("output", "cleaning_audit.csv"), show_col_types = FALSE)
}

# Fixed strings throughout, never regex: the figures are surrounded by markdown
# emphasis, and escaping asterisks for a regex is one more thing to get wrong
# in a file whose whole job is catching things that are wrong.
contains <- function(haystack, needle) grepl(needle, haystack, fixed = TRUE)

test_readme_states_the_committed_matching_coverage <- function() {
  a <- committed_audit()
  matched <- rule_rows(a, "Sales offset")
  credits <- rule_rows(a, "Credit notes")
  txt <- readme_text()

  stopifnot(contains(txt, sprintf("**%s**", format(matched, big.mark = ","))))
  stopifnot(contains(
    txt, sprintf("**%s** credit lines", format(credits, big.mark = ","))
  ))
  # The share is stated to one decimal and has to be that division, not a
  # third number typed beside the other two.
  stopifnot(contains(
    txt, sprintf("**%s** of them", percent(matched / credits, accuracy = 0.1))
  ))
}

test_readme_states_the_committed_row_counts <- function() {
  a <- committed_audit()
  txt <- readme_text()
  rows_in <- 541909
  rows_out <- rows_in - sum(a$rows_dropped)
  stopifnot(contains(
    txt, sprintf("**%s** clean sales lines", format(rows_out, big.mark = ","))
  ))
  stopifnot(contains(txt, sprintf("become %s clean", format(rows_out, big.mark = ","))))
}

test_readme_and_changelog_agree_on_the_headline <- function() {
  # The headline appears in the three-measure table and twice in the
  # reconciliation table.
  txt <- readme_text()
  changelog <- paste(readLines("CHANGELOG.md", warn = FALSE), collapse = "
")
  headline <- "9,861,394.40"
  stopifnot(length(gregexpr(headline, txt, fixed = TRUE)[[1]]) == 3)
  stopifnot(contains(changelog, headline))
  # The superseded figure is only ever mentioned as superseded.
  stopifnot(!contains(txt, "9,883,659.86"))
  stopifnot(contains(changelog, "9,883,659.86"))
}

test_the_reconciliation_table_is_internally_consistent <- function() {
  # gross - matched = net, as stated. Performed, not trusted.
  gross <- 10247353.28
  matched <- 385958.88
  net <- 9861394.40
  stopifnot(abs(gross - matched - net) < 0.005)

  # And the table claims the two projects agree, which means each row has the
  # SAME value in both columns. Checking that each figure appears somewhere is
  # not enough: changing one of a matched pair leaves the other behind and a
  # "does it appear" test passes while the table now says they differ. That is
  # what this file exists to catch, so the whole row is matched.
  txt <- readme_text()
  rows <- c(
    "| Gross positive product sales | £10,247,353.28 | £10,247,353.28 |",
    "| Value removed by matched credit notes | £385,958.88 | £385,958.88 |",
    "| Net of matched cancellations | **£9,861,394.40** | **£9,861,394.40** |"
  )
  for (row in rows) {
    stopifnot(contains(txt, row))
  }
}
