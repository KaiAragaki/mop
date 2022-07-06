#' Convert a pcr object into a tibble
#'
#' @param x a `pcr` object
#' @param include_header_and_footer Should the data from `x$header` and `x$footer` be included?
#' @param ... Unused
#'
#' @return a `tibble`
#' @export
scrub.pcr <- function(x, include_header_and_footer = FALSE, ...) {
  if (!x$is_tidy) {
    rlang::inform("Object is not tidy - tidying.")
    x <- tidy_lab(x)
  }
  data <- x$data
  tidy <- gp::gp_serve(data)

  if (include_header_and_footer) {
    header <- x$header |> t() |> tibble::as_tibble()
    colnames(header) <- janitor::make_clean_names(header)
    footer <- x$footer |> t() |> tibble::as_tibble()
    colnames(footer) <- janitor::make_clean_names(footer)
    tidy <- cbind(tidy, header) |> cbind(footer)
  }

  tidy |>
    dplyr::mutate(date = x$date,
                  experiment_type = x$experiment_type,
                  wells = x$wells)
}
