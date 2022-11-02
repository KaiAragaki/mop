#' Read an Incucyte File
#'
#' @param data Plotting data output from the Incucyte analysis
#' @param platemap .PlateMap file output from the platemap maker
#'
#' @return
#' @export

read_incucyte <- function(data, platemap) {
  read_data <- read_incucyte_data(data)
  read_platemap <- read_incucyte_platemap(platemap)

  dplyr::left_join(read_data, read_platemap, by = c(".row", ".col"))
}

read_incucyte_data <- function(x) {
  file_raw <- readr::read_file_raw(x)

  first <- readr::read_tsv(
    file_raw,
    skip_empty_rows = FALSE,
    show_col_types = FALSE
  )

  break_line <- which(is.na(first[[1]]))

  header <- readr::read_delim(
    file_raw,
    delim = ": ",
    col_names = FALSE,
    n_max = break_line,
    show_col_types = FALSE
  ) |>
    dplyr::mutate(X1 = snakecase::to_snake_case(.data$X1)) |>
    tidyr::pivot_wider(names_from = "X1", values_from = "X2")

  body <- readr::read_tsv(
    file_raw,
    skip = break_line,
    show_col_types = FALSE
  )

  body |>
    dplyr::rename(date_time = "Date Time",
                  elapsed = "Elapsed") |>
    tidyr::pivot_longer(cols = -c("date_time", "elapsed")) |>
    dplyr::mutate(.row = stringr::str_extract(name, "^[A-Z]"),
                  .col = stringr::str_extract(name, "[0-9]*$") |> as.numeric()) |>
    dplyr::rowwise() |>
    dplyr::mutate(.row = which(LETTERS == .data$.row)) |>
    dplyr::ungroup() |>
    dplyr::relocate(".row", ".col") |>
    dplyr::mutate(date_time = strptime(.data$date_time, format = "%m/%d/%Y %r")) |>
    dplyr::bind_cols(header)
}

read_incucyte_platemap <- function(x) {
  xml <- XML::xmlParse(x)
  row <- XML::xpathSApply(xml, "//well", XML::xmlGetAttr, "row")
  col <- XML::xpathSApply(xml, "//well", XML::xmlGetAttr, "col")
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
