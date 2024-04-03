#' Convert a pcr object into a tibble
#'
#' @param x a `pcr` object
#' @param include_header Should the data from `x$header` be included?
#' @param ... Unused
#'
#' @return a `tibble`
#' @export
scrub.pcr <- function(x, include_header = FALSE, ...) {
  if (!x$is_tidy) {
    rlang::inform("Object is not tidy - tidying.")
    x <- tidy_lab(x)
  }
  data <- x$data
  tidy <- gplate::gp_serve(data)

  # Add footer ----
  footer <- x$footer |> t() |> tibble::as_tibble()
  colnames(footer) <- janitor::make_clean_names(colnames(footer))
  tidy <- dplyr::bind_cols(tidy, footer)

  if (include_header) {
    header <- x$header |> t() |> tibble::as_tibble()
    colnames(header) <- janitor::make_clean_names(colnames(header))
    tidy <- dplyr::bind_cols(tidy, header)
  }

  tidy |>
    dplyr::mutate(date = x$date,
                  experiment_type = x$experiment_type,
                  wells = x$wells)
}
