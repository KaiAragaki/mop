#' Convert a spectramax object into a tibble
#'
#' @param x a `spectramax` object
#' @param n List item number to select data from
#' @param ... Unused
#'
#' @return a `tibble`
#' @export
scrub.spectramax <- function(x, n = 1, ...) {
  if (x$data[[n]]$type == "Plate") {
    x$data[[n]]$data |>
      gplate::gp_serve() |>
      dplyr::mutate(exp_date = x$date, exp_type = x$experiment_type, is_tidy = x$is_tidy)
  } else {
    x$data[[n]]$data |>
      dplyr::mutate(exp_date = x$date, exp_type = x$experiment_type, is_tidy = x$is_tidy)
  }
}
