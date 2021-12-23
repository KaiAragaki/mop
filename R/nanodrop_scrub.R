#' Convert a nanodrop object into a tibble
#'
#' @param x a `nanodrop` object
#' @param raw Should the data be pulled from `x$raw_data`?
#'
#' @return a `tibble`
#' @export
scrub.nanodrop <- function(x, raw = FALSE) {

  if (raw) data <- x$raw_data else data <- x$data

  dplyr::tibble(data, exp_date = x$date, nucleotide = x$nucleotide, is_tidy = x$is_tidy)
}
