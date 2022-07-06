#' Constructor for a pcr object
#'
#' @param data `data.frame` or `gp`, no restrictions on form.
#' @param raw_data `raw`, read in from the provided file with `readr::read_file_raw()`
#' @param date `lubridate::Date` object
#' @param experiment_type `character`. Could
#' @param is_tidy `logical`. Is the `data.frame` provided tidy?
#' @param header `data.frame`, the upper portion of the data before the body of
#'   the data. Usually contains run information.
#' @param footer `data.frame`, the lower portion of the data after the body of
#'   the data. Usually contains analysis information. May not exist.
#' @param wells Number of wells in plate
#'
#' @return A `pcr` object
#' @export
new_pcr <- function(data = data.frame(), raw_data = raw(), header = character(),
                    footer = character(), date = lubridate::Date(),
                    experiment_type = character(), wells = integer(), is_tidy = logical()) {
  stopifnot(is.data.frame(data) | class(data) == "gp",
            is.raw(raw_data))
  vec_assert(date, lubridate::Date())
  vec_assert(header, character())
  vec_assert(footer, character())
  vec_assert(experiment_type, character())
  vec_assert(wells, integer())
  vec_assert(is_tidy, logical())

  new_vctr(list(data = I(data), raw_data = raw_data, header = header,
                footer = footer, date = date,
                experiment_type = experiment_type, wells = wells, is_tidy = is_tidy),
           class = "pcr")
}

pcr <- function() {

}


#' @export
obj_print_data.pcr <- function(x, ...) {
  print(x$data)
}

#' @export
obj_print_footer.pcr <- function(x, ...) {
  cat(crayon::silver("# Experiment type:"),
      ifelse(!is.null(x$experiment_type), x$experiment_type, "NULL"),
      "\n")

  cat(crayon::silver("# Wells:"),
      ifelse(!is.null(x$wells), x$wells, "NULL"),
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


#' Convert a scrubbed pcr object back to a pcr object
#'
#' @param x A `tibble` - usually a previously scrubbed `pcr` object.
#'
#' @return A `pcr` object
#' @export
as_pcr <- function(x) {
  plate_dims <- gp::plate_formats[which(gp::plate_formats[1] == x$wells[1]), -1]
  data <- x |>
    dplyr::select(-.data$date, -.data$experiment_type, -.data$wells)
  new_pcr(data = gp::gp_unserve(data, nrow = plate_dims$rows, ncol = plate_dims$cols),
          date = lubridate::Date(x$date[1]),
          wells = x$wells[1],
          experiment_type = x$experiment_type[1],
          is_tidy = TRUE)
}

pcr_check <- function(x) {

}
