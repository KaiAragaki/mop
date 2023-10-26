#' Read and tidy a SPECTRAmax file
#'
#' @param path Path to the SPECTRAmax .txt file
#' @param date Date of experiment. If not supplied, uses current date.
#' @param wavelengths What wavelengths were read in this experiment?
#'
#' @details SPECTRAmax files cannot be read in easily without tidying them
#'   simultaneously, due to their non-rectangular structure. Therefore, tidying
#'   is _required_ to be read in and is not an option supplied.
#'
#' @return A list, with the following form: \describe{
#' \item{data}{a `tibble` that contains two columns - the `type`, and the (tidy)
#'   `data` itself in a list-column.}
#' \item{raw_data}{a `raw` representation of the file, before tidying}
#' \item{date}{character. The date of the experiment}
#' \item{experiment_type}{character. The type of experiment (currently only pq
#'   (Protein Quantification) and mtt supported)}
#' \item{tidy}{logical. Should always return TRUE if read in using `read_
#'   spectramax`}
#' }
#' @export
#' @example
#' system.file("extdata", "spectramax.txt", package = "mop") |>
#'   read_spectramax()
read_spectramax <- function(path, date = Sys.Date(), wavelengths = NULL) {
  rlang::inform("Please wait. This will take ~10 seconds.")
  ext <- fs::path_ext(path)
  raw <- readr::read_file_raw(path)

  if (ext == "txt") {
    out <- read_spectramax_txt(raw) # For those exporting from the .exe itself
    if (!is.null(wavelengths)) {
      wavelengths <- wavelengths
    } else {
      wavelengths <- lapply(out, \(x) x$wavelengths) |> unlist() |> unique()
    }
  } else if (ext %in% c("xls", "xlsx")) {
    # For 'copy paste into excel' method
    out <- read_spectramax_excel(path, wavelengths)
  } else {
    rlang::abort("Unknown file extension")
  }

  new_spectramax(
    data = out,
    raw_data = raw,
    date = Sys.Date(),
    wavelengths = as.numeric(wavelengths),
    is_tidy = TRUE
  )
}

read_spectramax_excel <- function(path, wavelengths) {
  x <- readxl::read_excel(path) |>
    dplyr::select(-1)
  x <- x[stats::complete.cases(t(x))] # Remove empty cols

  x <- x |>
    dplyr::mutate(.row = 1:nrow(x)) |>
    tidyr::pivot_longer(-.data$.row) |>
    dplyr::mutate(.col = stringr::str_extract(.data$name, "^[[:digit:]]*"),
                  .wavelength = stringr::str_extract(.data$name, "[^\\.]*$")) |>
    dplyr::group_by(.row, .col) |>
    dplyr::mutate(
      .wavelength = .data$.wavelength |> as.numeric() |> rank(),
      .wavelength = paste0("nm", wavelengths[.data$.wavelength])
    ) |>
    dplyr::select(-.data$name) |>
    tidyr::pivot_wider(
      names_from = .data$.wavelength, values_from = .data$value
    ) |>
    readr::type_convert()
  data <- gplate::gp(
    rows = max(x$.row), cols = max(x$.col), data = x, tidy = TRUE
  )
  list(data = data, type = "Plate", wavelengths = wavelengths) |> list()
}

read_spectramax_txt <- function(raw) {
  lines <- raw |>
    readr::read_file() |>
    strsplit("\r\n", useBytes = TRUE) |>
    unlist()

  block_start <- c(1, which(lines == "~End"))
  # The first blank line in a given section denotes the end of 'useful'
  # information (the rest tends to be footnotes). Unfortunately, that is not the
  # ONLY blank line in a section.
  blank_lines <- c(which(grepl("^\t*$", lines)))
  both <- c(block_start, blank_lines)
  both <- both[order(both)]
  # Get the next greatest hit in 'both' after a block_start
  block_end <- both[which(both %in% block_start) + 1]

  x <- tibble::tibble(skip = block_start + 1,
                      read_n = block_end - block_start - 3) |>
    dplyr::slice_head(n = -1)

  out <- purrr::map2(x$skip, x$read_n,
                     ~read_section(raw, n_skip = .x, n_read = .y)) |>
    purrr::map(tidy_section)
}

read_section <- function(raw, n_skip, n_read) {
  suppressMessages(
    header <- readr::read_tsv(
      raw, skip = n_skip - 1, n_max = 1,
      show_col_types = FALSE, skip_empty_rows = FALSE,
      col_names = FALSE, name_repair = "unique", guess_max = 10
    )
  )

  type <- header$X1 |> stringr::str_remove(":$")
  if (type != "Group") {
    wavelengths <- header$X16 |> stringr::str_split(" ") |> unlist()
  } else {
    wavelengths <- NULL
  }
  # Squashes irrelevant 'New Names...' msg
  suppressMessages(
    out <- readr::read_tsv(raw, skip = n_skip, n_max = n_read,
                           show_col_types = FALSE, skip_empty_rows = FALSE,
                           name_repair = "unique", guess_max = 10)
  )

  list(data = out, type = type, wavelengths = wavelengths)
}

tidy_section <- function(section) {
  if (section$type == "Plate") {
    data <- section$data[, -(1:2)]
    data <- data[stats::complete.cases(t(data))] # Remove hanging 'empty' cols
    data <- data |>
      dplyr::mutate(.row = 1:nrow(data)) |>
      tidyr::pivot_longer(-.data$.row) |>
      dplyr::mutate(
        .col = stringr::str_extract(.data$name, "^[[:digit:]]*"),
        .wavelength = stringr::str_extract(.data$name, "[^\\.]*$"
        )) |>
      dplyr::group_by(.row, .col) |>
      dplyr::mutate(
        .wavelength = .data$.wavelength |> as.numeric() |> rank(),
        .wavelength = paste0("nm", section$wavelengths[.data$.wavelength])
      ) |>
      dplyr::select(-.data$name) |>
      tidyr::pivot_wider(
        names_from = .data$.wavelength, values_from = .data$value
      )

    suppressMessages(data <- readr::type_convert(data))
    data <- gplate::gp(
      rows = max(data$.row), cols = max(data$.col), data = data, tidy = TRUE
    )
  }

  if (section$type == "Group") {
    names <- intersect(
      c("sample", "result", "mean_result", "std_dev",
        "cv_percent", "concentration", "mean_value"),
      colnames(section$data)
    )
    data <- tidyr::fill(section$data, names)
  }

  list(data = data, type = section$type, wavelengths = section$wavelengths)
}
