x <- "~/Desktop/2022-10-19_uc6-wound-assay-data.txt"

read_incucyte_data <- function(x) {
  file_raw <- readr::read_file_raw(x)
  first <- readr::read_tsv(file_raw, skip_empty_rows = F, show_col_types = F)
  break_line <- which(is.na(first[[1]]))
  header <- readr::read_delim(file_raw, delim = ": ", col_names = F, n_max = break_line)
  body <- readr::read_tsv(file_raw, skip = break_line)

  tidy_body <- body |>
    tidyr::pivot_longer(cols = c(-.data$`Date Time`, -.data$Elapsed)) |>
    dplyr::mutate(.row = stringr::str_extract(name, "^[A-Z]"),
                  .col = stringr::str_extract(name, "[0-9]*$")) |>
    dplyr::rowwise() |>
    dplyr::mutate(.row = which(LETTERS == .row)) |>
    dplyr::relocate(.row, .col)

}
