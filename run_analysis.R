# End-to-end driver for the R analysis. Run from the project root:
#
#     Rscript run_analysis.R
#
# Steps: check the raw data is present -> clean it (with a row-level audit) ->
# load the cleaned lines into SQLite and answer the business questions in SQL
# -> write the summary tables to output/ -> knit analysis.Rmd to analysis.md.
# Every artefact a reader sees in git is (re)produced by this one command.

source(file.path("R", "functions.R"))

message("== 1/4 Clean ==========================================")
cleaned <- cleaned_lines()
message(sprintf(
  "%s raw rows -> %s clean rows (%s dropped, %.1f%%)",
  format(cleaned$rows_in, big.mark = ","),
  format(cleaned$rows_out, big.mark = ","),
  format(cleaned$rows_in - cleaned$rows_out, big.mark = ","),
  100 * (cleaned$rows_in - cleaned$rows_out) / cleaned$rows_in
))

message("== 2/4 SQL ============================================")
con <- build_sqlite(cleaned$lines)
results <- run_queries(con)
dbDisconnect(con)
for (name in names(results)) {
  message(sprintf("  %-16s %4d rows", name, nrow(results[[name]])))
}

message("== 3/4 Write summary tables ===========================")
out_dir <- proj_path("output")
dir.create(out_dir, showWarnings = FALSE)
write_csv(cleaned$audit, file.path(out_dir, "cleaning_audit.csv"))
for (name in names(results)) {
  write_csv(results[[name]], file.path(out_dir, paste0(name, ".csv")))
}
message("  -> ", out_dir)

message("== 4/4 Knit report ====================================")
rmarkdown::render(
  proj_path("analysis.Rmd"),
  output_format = "github_document",
  quiet = TRUE
)
# github_document leaves a preview .html beside the .md; it is a local
# convenience only and stays out of git.
message("  -> ", proj_path("analysis.md"))

# Two portability fixes for the SVGs:
# - svglite resolves the requested font through fontconfig and writes whatever
#   THIS machine substituted (e.g. "Nimbus Sans"), which a reader's browser
#   won't have and would replace with a serif. Swap in a portable stack; the
#   metrics match because the local face is itself a Helvetica clone.
# - a dense polyline lands as one multi-kilobyte line, which chokes line-based
#   tooling (diffs, review UIs). Breaking at spaces is safe: inside an XML
#   attribute value a newline is just whitespace.
wrap_line <- function(line, width = 800) {
  out <- character()
  while (nchar(line) > width) {
    brk <- max(gregexpr(" ", substr(line, 1, width))[[1]])
    if (brk <= 0) break
    out <- c(out, substr(line, 1, brk - 1))
    line <- substr(line, brk + 1, nchar(line))
  }
  c(out, line)
}
for (svg in list.files(proj_path("analysis_files"), pattern = "\\.svg$",
                       recursive = TRUE, full.names = TRUE)) {
  content <- gsub('font-family: "[^"]*"', "font-family: Helvetica, Arial, sans-serif",
                  readLines(svg, warn = FALSE))
  writeLines(unlist(lapply(content, wrap_line)), svg)
}

message("Done.")
