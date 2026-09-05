# Fetch the UCI "Online Retail" transactions. Run from the project root:
#
#     Rscript R/get_data.R
#
# The file comes from the mirror in Databricks' "Spark: The Definitive Guide"
# repository because the UCI host is not reliably reachable, and is verified
# against a pinned sha256 before anything is allowed to read it.
#
# The digest check is the point, not the download: a truncated CSV parses
# fine and quietly changes every number downstream. Verify-then-rename means
# the final path only ever holds a complete, recognised file.

URL <- paste0(
  "https://raw.githubusercontent.com/databricks/Spark-The-Definitive-Guide/",
  "master/data/retail-data/all/online-retail-dataset.csv"
)
EXPECTED_SHA256 <- "a2f79bbdd4463df6db8a3f5a50b9c980ae8f645a370bf5e2c0d6097f9e817b05"
EXPECTED_BYTES <- 45038760

# Guard the working directory before anything touches the filesystem, or a
# run from elsewhere would quietly create data/raw/ somewhere unexpected.
if (!file.exists(file.path("R", "get_data.R"))) {
  stop("Run from the project root: Rscript R/get_data.R")
}

dest <- file.path("data", "raw", "online_retail.csv")

sha256_of <- function(path) {
  digest::digest(file = path, algo = "sha256", serialize = FALSE)
}

verify <- function(path) {
  size_ok <- file.size(path) == EXPECTED_BYTES
  digest <- sha256_of(path)
  size_ok && identical(digest, EXPECTED_SHA256)
}

# Fail before modifying an existing download if the required verifier is absent.
if (!requireNamespace("digest", quietly = TRUE)) {
  stop("Install the required SHA-256 verifier: install.packages('digest')")
}

if (file.exists(dest) && verify(dest)) {
  message("Already present and verified: ", dest)
} else {
  if (file.exists(dest)) {
    message(dest, " failed verification - re-downloading")
    unlink(dest)
  }
  dir.create(dirname(dest), showWarnings = FALSE, recursive = TRUE)
  part <- paste0(dest, ".part")
  message("Downloading -> ", dest)
  # unlink() before stop(), not on.exit(): at a script's top level there is no
  # function frame, so a top-level on.exit handler never fires and the partial
  # file would outlive the very error message claiming nothing was kept.
  status <- tryCatch(download.file(URL, part, mode = "wb", quiet = TRUE),
                     error = function(e) { message(conditionMessage(e)); 1L })
  if (status != 0 || !verify(part)) {
    unlink(part)
    stop(
      "Download failed verification (expected ", EXPECTED_BYTES, " bytes, sha256 ",
      substr(EXPECTED_SHA256, 1, 16), "...). Nothing was kept."
    )
  }
  file.rename(part, dest)
  message("Verified: ", dest)
}
