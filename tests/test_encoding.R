test_utf8_descriptions_round_trip <- function() {
  path <- tempfile(fileext = ".csv")
  on.exit(unlink(path))
  writeLines(enc2utf8(c(
    "InvoiceNo,StockCode,Description,Quantity,InvoiceDate,UnitPrice,CustomerID,Country",
    "1,gift_0001_20,Gift voucher £20,1,12/1/2010 8:26,20,12345,United Kingdom"
  )), path, useBytes = TRUE)
  raw <- read_retail_raw(path)
  stopifnot(identical(raw$description, "Gift voucher £20"))
}
