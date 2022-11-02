#' Tidy an incucyte object
#'
#' @param x An `incucyte` object to tidy
#'
#' @param force_tidy `logical`. Should the tidying take place, even if the
#'   `is_tidy` attribute is `TRUE`?
#' @param ... Unused
#'
#' @return an `incucyte` object
#'
#' @details This function:
#'
#' @importFrom rlang .data
#' @export
tidy_lab.incucyte_data <- function(x, force_tidy = FALSE, ...) {

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
    dplyr::mutate(date = lubridate::mdy_hms(.data$date) |> as.character())

  new_nanodrop(data = data, raw_data = x$data, nucleotide = x$nucleotide, is_tidy = TRUE, date = x$date)
}

tidy_lab.incucyte_platemap <- function(x) {
  xml <- XML::xmlParse(x)
  row <- XML::xpathSApply(x, "//well", xmlGetAttr, "row")
  col <- XML::xpathSApply(x, "//well", xmlGetAttr, "col")
  wells <- dplyr::tibble(row, col)

  out <- purrr::map2(row, col, tidy_well, xml = xml) |>
    dplyr::bind_rows()
}

tidy_well <- function(xml, row, col) {
  q <- paste0("//well[@row='", row, "' and @col='", col, "']/items/wellItem")
  q_res <- XML::xpathSApply(xml, q, tidy_attrs, simplify = F)
  r <- paste0(q, "/referenceItem")
  r_res <- XML::xpathSApply(xml, r, tidy_attrs, simplify = F)

  mapply(rbind, q_res, r_res, SIMPLIFY = F) |>
    lapply(tidy_df) |>
    dplyr::bind_rows() |>
    dplyr::mutate(.row = as.numeric(row) + 1, .col = as.numeric(col) + 1) |>
    dplyr::relocate(.row, .col) |>
    dplyr::group_by(.row, .col) |>
    tidyr::fill(dplyr::everything(), .direction = "downup") |>
    dplyr::distinct() |>
    dplyr::ungroup()
}

tidy_attrs <- function(x) {
  x |>
    XML::xmlAttrs() |>
    dplyr::as_tibble(rownames = "desc")
}

tidy_df <- function(x) {
  filtered_x <- x |>
    dplyr::distinct() |>
    dplyr::filter(desc != "colorArgb") # Color doesn't mean much here

  name <- filtered_x[which(filtered_x$desc == "type"), 2][[1]] |>
    stringr::str_replace_all(" ", "_")

  filtered_x |>
    dplyr::filter(desc != "type") |>
    dplyr::mutate(desc = paste0(name, "_", desc)) |>
    tidyr::pivot_wider(names_from = desc, values_from = value)
}
