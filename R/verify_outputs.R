# Compare committed numeric evidence with current code before CI overwrites it.
source(file.path("R", "functions.R"))
cleaned <- cleaned_lines()
con <- build_sqlite(cleaned$lines)
expected <- run_queries(con)
dbDisconnect(con)
expected$cleaning_audit <- cleaned$audit
for (name in names(expected)) {
  published <- readr::read_csv(file.path("output", paste0(name, ".csv")),
                              show_col_types = FALSE)
  actual <- expected[[name]]
  stopifnot(identical(names(published), names(actual)), nrow(published) == nrow(actual))
  for (column in names(actual)) {
    # CSV does not preserve R integer/double distinctions or Date classes.
    left <- published[[column]]
    right <- actual[[column]]
    if (is.numeric(left) && is.numeric(right)) {
      agrees <- isTRUE(all.equal(as.numeric(left), as.numeric(right), tolerance = 1e-9))
    } else {
      agrees <- identical(as.character(left), as.character(right))
    }
    if (!agrees) stop("Published output is stale: ", name, "/", column,
                     ". Rebuild with Rscript run_analysis.R and commit the outputs.")
  }
}
message("All committed summary tables agree with the current pipeline.")
