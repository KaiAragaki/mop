#' Get or set an object's plate data
#'
#' @param x Object to extract plate data from
#' @param ... Arguments to be passed to their respective methods
#'
#' @return A `gp` object
#' @export
plate_data <- function(x, ...) {
  UseMethod("plate_data")
}

#' @param value Value to set the plate to.
#' @rdname plate_data
#' @export
'plate_data<-' <- function(x, value) {
  UseMethod("plate_data<-", x)
}
