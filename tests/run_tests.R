# Test runner: sources the shared functions and every test file, then calls
# each function whose name starts with test_. Exits non-zero on any failure so
# CI can gate on it.
#
#     Rscript tests/run_tests.R    (from the project root)

source(file.path("R", "functions.R"))

test_files <- list.files("tests", pattern = "^test_.*\\.R$", full.names = TRUE)
for (f in test_files) source(f)

tests <- Filter(function(x) is.function(get(x)), ls(pattern = "^test_"))
failures <- 0
for (t in tests) {
  result <- tryCatch({
    get(t)()
    "ok"
  }, error = function(e) conditionMessage(e))
  if (identical(result, "ok")) {
    message("PASS ", t)
  } else {
    failures <- failures + 1
    message("FAIL ", t, ": ", result)
  }
}

message(sprintf("%d/%d tests passed", length(tests) - failures, length(tests)))
if (failures > 0) quit(status = 1)
