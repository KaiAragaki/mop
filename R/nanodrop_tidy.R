#' Tidy a nanodrop object
#'
#' @param x A `nanodrop` object to tidy
#'
#' @param force_tidy `logical`. Should the tidying take place, even if the
#'   `is_tidy` attribute is `TRUE`?
#'
#' @return a `nanodrop` object
#'
#' @details This function:
#'
#'   * renames columns to sensible substitutes
#'
#'   * converts date-times to ISO 8601-esque date-time format (YYYY-MM-DD
#'   HH:MM:SS vs YYYY-MM-DDTHH:MM:SSZ)
#'
#'   It's recommended that you do any manipulations to these data after you
#'   tidy, rather than before, as `tidy_nanodrop` expects the output to be
#'   fairly similar to the output from `read_nanodrop`.
#'
#'   A tidy `nanodrop` object will (usually) contain the following columns:
#'
#'   * `date` The date-time of sample reading, as YYYY-MM-DD HH:MM:SS
#'
#'   * `sample_name` The name of the sample provided by the NanoDrop itself
#'
#'   * `conc` The concentration of nucleic acid in ng/uL
#'
#'   * `a260_280` The absorbance at 260nm / absorbance at 280nm. Typically a
#'   marker of protein contamination. Pure nucleic acid is typically around 2.
#'
#'   * `a260_230` The absorbance at 260nm / absorbance at 230nm. Typically a
#'   marker of guanadine salt contamination. Pure nucleic acid is typically
#'   around 2.
#'
#'   * `a260` The absorbance at 260nm, the wavelength nucleic acids absorb most
#'   strongly.
#'
#'   * `a280` The absorbance at 280nm, the wavelength proteins absorb most
#'   strongly.
#'
#'   * `tube_name` The label given the physical tube that contains sample
#'
#'   * `cell_line` The cell line from which the sample came, if applicable
#'
#'   * `experimental_condition` A brief description of the conditions these
#'   samples were subject to for the experiment
#'
#'
#'   `tube_name`, `cell_line`, and `experimental_condition` are always unfilled,
#'   because these data are not within nanodrop data. However, they are provided
#'   because they are included in the masterlist data, and it's wise to fill
#'   them out for your records.
#'
#'   The remaining columns are typically unused.
#'
#' @importFrom rlang .data
#' @export
tidy_lab.nanodrop <- function(x, force_tidy = FALSE, ...) {

  if (x$is_tidy && !force_tidy) {
    message("nanodrop already looks tidy, skipping. To tidy anyway, set force_tidy = TRUE.")
    return(x)
  }

  data <- x$data |>
    dplyr::rename_with(dplyr::recode,
                       Nucleic.Acid.ng.uL. = "conc",
                       A260.A280 = "a260_280",
                       A260.A230 = "a260_230"
    ) |>
    dplyr::rename_with(snakecase::to_snake_case) |>
    dplyr::rename_with(stringr::str_replace, .cols = dplyr::everything(), "_u_l", "ul") |>
    dplyr::rename_with(stringr::str_replace, .cols = dplyr::everything(), "a_260", "a260") |>
    dplyr::rename_with(stringr::str_replace, .cols = dplyr::everything(), "a_280", "a280") |>
    dplyr::mutate(date = lubridate::mdy_hms(.data$date) |> as.character(),
                  tube_name = "",
                  cell_line = "",
                  experimental_condition = "")

  new_nanodrop(data = data, raw_data = x$data, nucleotide = x$nucleotide, is_tidy = TRUE, date = x$date)
}
