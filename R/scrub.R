#' Convert a lab object to a tidy tibble
#'
#' Objects are a convenient way to represent lab data as it allows downstream
#' functions to interact with them in specific ways depending on their type.
#' Sometimes, however, it is more convenient to work with a flat `tibble`.
#' `scrub` converts a lab object into a `tibble`
#'
#' @param x object to be converted into a `tibble`
#' @param ... additional arguments to pass on to the respective object's class method
#'
#' @return a `tibble`
#' @export
scrub <- function(x, ...) {
  UseMethod("scrub")
}
