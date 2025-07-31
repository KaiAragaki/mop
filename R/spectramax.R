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

#' @rdname plate_data
#' @export
plate_data.spectramax <- function(x, ...) {
  stopifnot(x$data[[1]]$type == "Plate")
  x$data[[1]]$data
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
