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
  plate_dims <- gplate::plate_formats[which(gp::plate_formats[1] == x$wells[1]), -1]

  footer_names <- c("analysis_type", "endogenous_control", "rq_min_max_confidence_level", "reference_sample")
  header_names <- c("block_type", "calibration_background_is_expired",
                    "calibration_background_performed_on", "calibration_normalization_fam_rox_is_expired",
                    "calibration_normalization_fam_rox_performed_on", "calibration_normalization_vic_rox_is_expired",
                    "calibration_normalization_vic_rox_performed_on", "calibration_pure_dye_fam_is_expired",
                    "calibration_pure_dye_fam_performed_on",
                    "calibration_pure_dye_ned_is_expired", "calibration_pure_dye_ned_performed_on",
                    "calibration_pure_dye_rox_is_expired", "calibration_pure_dye_rox_performed_on",
                    "calibration_pure_dye_sybr_is_expired", "calibration_pure_dye_sybr_performed_on",
                    "calibration_pure_dye_tamra_is_expired", "calibration_pure_dye_tamra_performed_on",
                    "calibration_pure_dye_vic_is_expired", "calibration_pure_dye_vic_performed_on",
                    "calibration_roi_is_expired", "calibration_roi_performed_on",
                    "calibration_uniformity_is_expired", "calibration_uniformity_performed_on",
                    "chemistry", "date_created", "experiment_barcode", "experiment_comment",
                    "experiment_file_name", "experiment_name", "experiment_run_end_time",
                    "experiment_type", "instrument_name", "instrument_serial_number",
                    "instrument_type", "passive_reference", "quantification_cycle_method",
                    "signal_smoothing_on", "stage_cycle_where_analysis_is_performed", "user_name")

  data <- x |>
    dplyr::select(-.data$date, -.data$experiment_type, -.data$wells, -dplyr::any_of(c(footer_names, header_names)))

  footer <- dplyr::select(x, dplyr::any_of(footer_names)) |> dplyr::slice(1) |> t() |> tibble::as_tibble(rownames = "name", .name_repair = "minimal") |> tibble::deframe()
  header <- dplyr::select(x, dplyr::any_of(header_names)) |> dplyr::slice(1) |> t() |> tibble::as_tibble(rownames = "name", .name_repair = "minimal") |> tibble::deframe()

  new_pcr(data = gplate::gp_unserve(data, nrow = plate_dims$rows, ncol = plate_dims$cols),
          date = lubridate::as_date(x$date[1]),
          wells = x$wells[1],
          experiment_type = x$experiment_type[1],
          header = header,
          footer = footer,
          is_tidy = TRUE)
}
