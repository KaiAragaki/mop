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
  stopifnot(is.list(data),
            is.raw(raw_data))
  vec_assert(date, lubridate::Date())
  vec_assert(wavelengths, numeric())
  vec_assert(is_tidy, logical())
  new_vctr(list(data = data, raw_data = raw_data, date = date,
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
  cat(crayon::silver("# Wavelengths:"),
      ifelse(!is.null(x$wavelengths), x$wavelengths, "NULL"),
      "\n")

  cat(crayon::silver("# Is tidy:"),
      dplyr::case_when(is.null(x$is_tidy) ~ crayon::silver("NULL"),
                       x$is_tidy == TRUE ~ crayon::blue(x$is_tidy),
                       x$is_tidy == FALSE ~ crayon::red(x$is_tidy)),
      "\n")

  cat(crayon::silver("# Date:"),
      ifelse(!is.null(x$date), as.character(x$date), "NULL"),
      "\n")
}
