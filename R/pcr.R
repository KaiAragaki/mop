#' Constructor for a pcr object
#'
#' @param data `data.frame`, no restrictions on form.
#' @param raw_data `data.frame`, no restrictions on form. Typically represents
#'   read-in data with no changes to it.
#' @param date `lubridate::Date` object
#' @param experiment_type `character`. Could
#' @param is_tidy `logical`. Is the `data.frame` provided tidy?
#' @param header `data.frame`, the upper portion of the data before the body of
#'   the data. Usually contains run information.
#' @param footer `data.frame`, the lower portion of the data after the body of
#'   the data. Usually contains analysis information. May not exist.
#'
#' @return
#' @export
#'
#' @examples
new_pcr <- function(data = data.frame(), raw_data = data.frame(), header = data.frame(),
                    footer = data.frame(), date = lubridate::Date(),
                    experiment_type = character(), is_tidy = logical()) {
  stopifnot(is.data.frame(data),
            is.data.frame(raw_data),
            is.data.frame(header),
            is.data.frame(footer))
  vec_assert(date, lubridate::Date())
  vec_assert(experiment_type, character())
  vec_assert(is_tidy, logical())

  new_vctr(list(data = I(data), raw_data = I(raw_data), header = I(header),
                footer = I(footer), date = date,
                experiment_type = experiment_type, is_tidy = is_tidy),
           class = "pcr")
}
