#' Constructor for a synergy2 object
#'
#' @param data a `data.frame`, no restrictions on form.
#' @param wavelengths a num
#' @param metadata a `list`, no restrictions on form
#'
#' @return a `synergy2` object
#' @export
new_synergy2 <- function(data = data.frame(),
                         wavelengths = numeric(),
                         metadata = list()) {
  stopifnot(is.list(data), is.numeric(wavelengths), is.list(metadata))
  structure(
    list(data = data, wavelengths = wavelengths, metadata = metadata),
    class = "synergy2"
  )
}

synergy2 <- function() {

}

#' @rdname plate_data
#' @export
plate_data.synergy2 <- function(x, ...) {
  x$data
}

#' @rdname plate_data
#' @export
'plate_data<-.synergy2' <- function(x, value) {
  x$data <- value
  x
}
