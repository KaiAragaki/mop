#' Constructor for a spectramax object
#'
#' @param data a list of `data.frame`s, no restrictions on form.
#' @param raw_data `raw`. Typically represents read-in data with no changes to it.
#' @param date `lubridate::Date` object
#' @param experiment_type `character`, no restrictions on form
#' @param is_tidy `logical`. Are the `data` provided tidy?
#'
#' @return a `spectramax` object
#' @export
new_spectramax <- function(data = list(), raw_data = raw(), date = lubridate::Date(),
                           experiment_type = character(), is_tidy = logical()) {
  stopifnot(is.list(data),
            is.raw(raw_data))
  vec_assert(date, lubridate::Date())
  vec_assert(experiment_type, character())
  vec_assert(is_tidy, logical())
  new_vctr(list(data = data, raw_data = raw_data, date = date,
                experiment_type = experiment_type, is_tidy = is_tidy),
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
  cat(crayon::silver("# Experiment type:"),
      ifelse(!is.null(x$experiment_type), x$experiment_type, "NULL"),
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
