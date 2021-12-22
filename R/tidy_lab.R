#' Tidy a lab object
#'
#' @param x An object to tidy
#' @param ... Arguments passed to their respective methods
#'
#' @export
tidy_lab <- function(x, ...) {
  UseMethod("tidy_lab")
}
