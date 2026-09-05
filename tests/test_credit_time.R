test_credit_never_cancels_future_sale <- function() {
  d <- fixture()[c(1, 2, 3), ]
  d$invoice_date <- c("12/1/2010 8:00", "12/1/2010 10:00", "12/1/2010 9:00")
  res <- clean_retail(d)
  stopifnot(identical(res$lines$invoice_no, "536366"))
}

test_unmatched_early_credit_does_not_consume_later_sale <- function() {
  d <- fixture()[c(1, 3), ]
  d$invoice_date <- c("12/1/2010 10:00", "12/1/2010 9:00")
  res <- clean_retail(d)
  stopifnot(res$rows_out == 1, res$audit$rows_dropped[2] == 0)
}

test_chronological_credits_match_each_sale_once <- function() {
  d <- fixture()[c(1, 2, 3, 3), ]
  d$invoice_no <- c("S1", "S2", "C1", "C2")
  d$invoice_date <- c("12/1/2010 8:00", "12/1/2010 10:00",
                      "12/1/2010 9:00", "12/1/2010 11:00")
  res <- clean_retail(d[c(4, 2, 3, 1), ])
  stopifnot(res$rows_out == 0, res$audit$rows_dropped[2] == 2)
}

test_same_minute_credit_is_eligible <- function() {
  d <- fixture()[c(1, 3), ]
  d$invoice_date <- rep("12/1/2010 8:00", 2)
  stopifnot(clean_retail(d)$rows_out == 0)
}

test_sha256_detects_same_size_same_timestamp_change <- function() {
  path <- tempfile()
  on.exit(unlink(path))
  writeBin(charToRaw("abc"), path)
  stamp <- file.mtime(path)
  first <- file_sha256(path)
  stopifnot(identical(first, "ba7816bf8f01cfea414140de5dae2223b00361a396177a9cb410ff61f20015ad"))
  writeBin(charToRaw("abd"), path)
  Sys.setFileTime(path, stamp)
  stopifnot(!identical(first, file_sha256(path)))
}
