#' Read and tidy a SPECTRAmax file
#'
#' @param path Path to the SPECTRAmax .txt file
#' @param date Date of experiment. If not supplied, uses current date.
#' @param experiment_type What kind of experiment? Either 'pq' for protein
#'   quantification, or mtt
#'
#' @details SPECTRAmax files cannot be read in easily without tidying them
#'   simultaneously, due to their non-rectangular structure. Therefore, tidying
#'   is _required_ to be read in and is not an option supplied.
#'
#' @return A list, wih the following form: \describe{ \item{data}{a `tibble`
#'   that contains two columns - the `type`, and the (tidy) `data` itself in a list-column.}
#'   \item{raw_data}{a `raw` representation of the file, before tidying}
#'   \item{date}{character. The date of the experiment}
#'   \item{experiment_type}{character. The type of exeperiment (currently only
#'   pq (Protein Quantification) and mtt supported)} \item{tidy}{logical. Should
#'   always return TRUE if read in using `read_spectramax`} }
#' @export
#'
#' @example
#' system.file("extdata", "spectramax.txt", package = "mop") |>
#'   read_spectramax()
read_spectramax <- function(path, date = Sys.Date(), experiment_type = c("pq", "mtt")) {

  experiment_type <- rlang::arg_match(experiment_type)

  if (fs::path_ext(path) == "xls" | fs::path_ext(path) == "xlsx") {
    x <- readxl::read_excel(path) |>
      dplyr::select(-1) |>
      rlang::set_names(NULL) |>
      gp::as_gp()
    x <- tibble::tibble(type = "plate", data = list(x))
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
      dplyr::mutate(clean = purrr::map2(out, types, tidy_spectramax)) |>
      dplyr::select(type = types, data = clean)
  }

  new_spectramax(
    data = x,
    raw_data = readr::read_file_raw(path),
    date = Sys.Date(),
    experiment_type = experiment_type,
    is_tidy = TRUE
  )
}

tidy_spectramax <- function(data, type) {
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
