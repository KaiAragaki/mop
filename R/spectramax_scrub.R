#' Convert a spectramax object into a tibble
#'
#' @param x a `spectramax` object
#' @param ... Unused
#'
#' @return a `tibble`
#' @export
scrub.spectramax <- function(x, ...) {
  data <- x$data
  if (sum(data$type == "plate") > 1) {
    rlang::inform("There are multiple plates in this dataset. Only the first will be selected.")
  }
  plate <- data[which(data$type == "plate")[1], 2][[1]][[1]]
  gp::gp_serve(plate) |>
    dplyr::mutate(exp_date = x$date, exp_type = x$experiment_type, is_tidy = x$is_tidy)
}
