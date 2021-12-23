#' Read in a NanoDrop file
#'
#' @param path path to a NanoDrop .csv
#' @param nucleotide An optional character vector describing the analyte
#'   detected. If NULL (the default), it will automatically try to extract one
#'   from the file path. Valid values include RNA, DNA, dsDNA, and dsRNA (or
#'   NULL).
#' @param date An optional character vector coercible to a date object. If NULL
#'   (the default), it will automatically try to extract one from the file path.
#'   See details for detection methods.
#' @param date_order An optional character vector to enforce an order of date to
#'   be read (if `date` is unspecified). Can be "ymd", "mdy", or "dmy"
#'
#' @return a `nanodrop` object
#'
#' @details  This function expects a file that has not been tampered with from
#'   the nanodrop machine itself, aside for the name.
#'
#'   This function calls `read.delim` and guesses its encoding (usually
#'   `UTF-16LE`, unless it's been re-exported). It's less mysterious than reading
#'   in an apparent `.csv` with `read.delim`, and it keeps you from having to
#'   remember the encoding (which R often fails to auto-detect). When possible,
#'   it attempts to extract the nucleotide type from the path provided.
#'
#'   if `date` is `NULL`, the function will attempt to extract a date from the
#'   file name (not the whole file path). It will look for ymd, mdy, and dmy (in
#'   order). If it detects the first format, it will not look for the second,
#'   etc. Note how a reading taken from April 4th but written as 4_3_2021 will
#'   be misinterpreted. This error can be remedied by specifying the argument
#'   `date_order`
#'
#' @export
#'
#' @examples
#' system.file("extdata", "nanodrop.csv", package = "mop") |>
#'   read_nanodrop()
read_nanodrop <- function(path, nucleotide = NULL, date = NULL, date_order = NULL) {

  if (is.null(nucleotide)) {
    nucleotide <- stringr::str_extract(fs::path_file(path), "(ds)?[D|R]NA")
    if (is.na(nucleotide)) nucleotide <- character(length = 1L)
  }

  if (is.null(date) && is.null(date_order)) {

    # First, try for YYYY-MM-DD
    date <- stringr::str_extract(fs::path_file(path), "[:digit:]{4}[-|_][:digit:]{2}[-|_][:digit:]{2}") |>
      lubridate::ymd()

    # If that fails, try MM-DD-YYYY
    if (is.na(date)) {
      date <- stringr::str_extract(fs::path_file(path), "[:digit:]{1,2}[-|_][:digit:]{1,2}[-|_][:digit:]{4}") |>
        lubridate::mdy()
    }

    # And finally, if that fails, try DD-MM-YYYY
    if (is.na(date)) {
      date <- stringr::str_extract(fs::path_file(path), "[:digit:]{1,2}[-|_][:digit:]{1,2}[-|_][:digit:]{4}") |>
        lubridate::dmy()
    }
  }

  if (is.null(date) && !is.null(date_order)) {
    if (date_order == "ymd") {
      date <- stringr::str_extract(fs::path_file(path), "[:digit:]{4}[-|_][:digit:]{2}[-|_][:digit:]{2}") |>
        lubridate::ymd()
    } else {
      date <- stringr::str_extract(fs::path_file(path), "[:digit:]{1,2}[-|_][:digit:]{1,2}[-|_][:digit:]{4}") |>
        lubridate::parse_date_time(date_order) |>
        lubridate::as_date()
    }
  }

  if (is.na(date)) date <- lubridate::Date(length = 1L)

  data <- utils::read.delim(path, fileEncoding = readr::guess_encoding(path)$encoding[1]) |>
    dplyr::as_tibble() |>
    suppressWarnings() # These files often lack EOL characters. This squelches that error associated with that.

  new_nanodrop(data = data, raw_data = data, nucleotide = nucleotide, is_tidy = FALSE, date = date) |>
    validate_nanodrop()
}
