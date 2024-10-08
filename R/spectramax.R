#' Constructor for a spectramax object
#'
#' @param data a `data.frame`, no restrictions on form.
#' @param raw_data `raw`, read in from the provided file with `readr::read_file_raw()`
#' @param date `lubridate::Date` object
#' @param experiment_type `character`, no restrictions on form
#' @param is_tidy `logical`. Are the `data` provided tidy?
#'
#' @return a `spectramax` object
#' @export
new_spectramax <- function(data = data.frame(), raw_data = raw(), date = lubridate::Date(),
                           wavelengths = numeric(), is_tidy = logical()) {
  stopifnot(is.list(data))
  vec_assert(date, lubridate::Date())
  vec_assert(wavelengths, numeric())
  vec_assert(is_tidy, logical())
  new_vctr(list(data = data, date = date,
                wavelengths = wavelengths, is_tidy = is_tidy),
           class = "spectramax")
}

spectramax <- function() {

}

#' @export
obj_print_data.spectramax <- function(x, ...) {
  print(x$data)
}

#' @export
obj_print_footer.spectramax <- function(x, ...) {
  cat(crayon::silver("# Date:"),
      ifelse(!is.null(x$date), as.character(x$date), "NULL"),
      "\n")
}


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

#' @rdname plate_data
#' @export
plate_data.spectramax <- function(x, ...) {
  stopifnot(x$data[[1]]$type == "Plate")
  x$data[[1]]$data
}

#' @param value Value to set the plate to.
#' @rdname plate_data
#' @export
'plate_data<-' <- function(x, value) {
  UseMethod("plate_data<-", x)
}

#' @rdname plate_data
#' @export
'plate_data<-.spectramax' <- function(x, value) {
  x$data[[1]]$data <- value
  x
}

#' @export
gp_sec.spectramax <- function(x, ...) {
  pd <- plate_data(x)
  plate_data(x) <- gp_sec(pd, ...)
  x
}
