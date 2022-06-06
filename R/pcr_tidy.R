#' Tidy a PCR object
#'
#' @param x a `pcr` object
#' @param usr_standards Custom supplied standards
#' @param pad_zero Should, say, Sample 1 become Sample 01?
#' @param ...
#'
#' @return
#' @export
tidy_lab.pcr <- function(x, usr_standards = NULL, pad_zero = FALSE, ...) {

  colnames(x$data) <- snakecase::to_snake_case(colnames(x$data))

  # Using 'startsWith' since the experiment type contains non-ascii chars
  if (startsWith(x$experiment_type, "Comparative")) {
    x$experiment_type <- "comparative_ct"
  }

  if (x$experiment_type == "Standard Curve") {
    x$experiment_type <- "standard_curve"
  }

  x$data <- x$data |>
    dplyr::mutate(.row = stringr::str_extract(.data$well_position, "^.{1}"),
                  .col = as.numeric(stringr::str_extract(.data$well_position, "[:digit:]{1,2}$")),
                  .row = as.numeric(factor(.data$.row, levels = LETTERS)),
                  ct = as.numeric(ct) |> suppressWarnings()) |>
    dplyr::relocate(.row, .col)

  if (x$experiment_type == "standard_curve") {

    standards <- x$data |>
      dplyr::filter(task == "STANDARD") |>
      dplyr::arrange(quantity) |> # for findInterval
      dplyr::pull(quantity) |>
      unique()

    if (is.null(usr_standards)) {
      usr_standards <- 10^(0:-4) * 6
    }

    usr_standards <- usr_standards |>
      tibble::enframe(name = "name", value = "usr_quantity") |>
      dplyr::mutate(name = paste("Standard", seq_len(length(usr_standards)))) |>
      dplyr::arrange(usr_quantity)

    if (nrow(usr_standards) > length(standards)) {
      rlang::abort("More standards supplied than exist in dataset")
    }

    if (max(usr_standards$usr_quantity) >= max(standards)) {
      rlang::abort(message = c("Largest standard supplied is greater than largest standard in dataset.",
                               i = "Supplied standards are rounded up."))
    }

    fi_standards <- standards * 1.000001 # in case largest usr_stan = stan, this lets it get in between the interval

    intervals <- findInterval(usr_standards$usr_quantity, fi_standards)
    positions <- intervals + 1

    final <-
      tibble::tibble(positions,
                     quantity = standards[positions],
                     task = "STANDARD") |>
      dplyr::bind_cols(usr_standards)

    x$data <- dplyr::left_join(x$data, final, by = c("task", "quantity")) |>
      dplyr::mutate(sample_name = ifelse(is.na(sample_name), name, sample_name))

    dropping <- x$data |>
      dplyr::filter(is.na(sample_name) & task == "STANDARD")

    if (nrow(dropping) > 0) {
      x$data <- x$data |>
        dplyr::filter(!is.na(sample_name) | task != "STANDARD")
      message(nrow(dropping), " rows of standards did not have a matching value in 'standards' and have been dropped")
      x$data <- pcr_calc_slope(x$data)
    }
  }

  if (pad_zero) {
    x$data$sample_name <- pad_zero(x$data$sample_name)
  }

  if (x$wells == 384) {
    x$data <- gp::gp_unserve(x$data, nrow = 16, ncol = 24)
  } else if (x$wells == 96) {
    x$data <- gp::gp_unserve(x$data, nrow = 8, ncol = 12)
  } else {
    x$data <- gp::gp_unserve(x$data)
  }

  x$is_tidy <- TRUE

  x

}
