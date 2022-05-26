spectramax_read <- function(path, date = Sys.Date(), experiment_type = c("pq", "mtt")) {

  if (fs::path_ext(path) == "xls" | fs::path_ext(path) == "xlsx") {
    x <- readxl::read_excel(path) |>
      dplyr::select(-1) |>
      rlang::set_names(NULL) |>
      gp::as_gp()
  }

  if (fs::path_ext(path) == "txt") {
    lines <- path |>
      readr::read_file() |>
      strsplit("\r\n") |>
      unlist()
    block_start <- c(1, which(lines == "~End"))
    blank_lines <- c(which(grepl("^\t*$", lines)))
    both <- c(block_start, blank_lines)
    both <- both[order(both)]
    block_end <- both[which(both %in% block_start) + 1]
    x <- tibble::tibble(skip = block_start + 1,
                        read_n = block_end - block_start - 3) |>
      dplyr::slice_head(n = -1)
    types <-
      purrr::map(block_start[-length(block_start)],
                 ~readr::read_tsv(path, skip = .x, n_max = 1, col_names = FALSE,
                                  show_col_types = FALSE, skip_empty_rows = FALSE)) |>
      lapply(\(x) x[1,1]) |>
      unlist() |>
      tolower() |>
      stringr::str_remove(":$")
    out <-
      purrr::map2(x$skip, x$read_n,
                  ~readr::read_tsv(path, skip = .x, n_max = .y,
                                   show_col_types = FALSE, skip_empty_rows = FALSE)) |>
      lapply(\(x) setNames(x, janitor::make_clean_names(colnames(x))))

    x <-
      tibble::tibble(out, types) |>
      dplyr::mutate(clean = purrr::map2(out, types, spectramax_tidy)) |>
      dplyr::select(type = types, data = clean)
  }

  new_spectramax(data = x, raw_data = readr::read_file_raw(path), date = Sys.Date(), experiment_type = experiment_type, is_tidy = TRUE)
}

spectramax_tidy <- function(data, type) {
  if (type == "plate") {
    data <- data[,-(1:2)]
    data <- data[stats::complete.cases(t(data))] # Remove hanging 'empty' cols
    data <- gp::as_gp(data)
  }

  if (type == "group") {
    names <- intersect(c("sample", "result", "mean_result", "std_dev",
                         "cv_percent", "concentration", "mean_value"),
                       colnames(data))
    data <- tidyr::fill(data, names)
  }

  data
}
