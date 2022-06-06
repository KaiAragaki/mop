#' Read in a QuantStudio pcr file
#'
#' @param path path to a QuantStudio .xls
#'
#' @return a `pcr` object
#' @export
read_pcr <- function(path) {

  # .name_repair argument silences the renaming business
  raw_data <-
    readxl::read_excel(
      path = path.expand(path),
      sheet = "Results",
      col_names = FALSE,
      col_types = "guess",
      .name_repair = ~ vctrs::vec_as_names(..., repair = "unique", quiet = TRUE)
    )

  breaks <- which(is.na(raw_data[,1]))
  stopifnot(length(breaks) %in% 1:2)

  header <- raw_data |>
    dplyr::slice_head(n = (breaks[1] - 1)) |>
    dplyr::select(1:2) |>
    tibble::deframe()

  if (length(breaks) == 2) {
    body <- dplyr::slice(raw_data, (breaks[1]+1):(breaks[2]-1))
    footer <- raw_data |>
      dplyr::slice((breaks[2]+1):nrow(raw_data)) |>
      dplyr::select(1:2) |>
      tibble::deframe()
  }

  # These files don't always have footers
  if (length(breaks) == 1) {
    body <- dplyr::slice(raw_data, (breaks[1]+1):nrow(raw_data))
    footer <- character()
  }

  # Turn the first row into colnames, then remove that row. Convert cols into
  # their proper types
  colnames(body) <- body[1,]
  body <- body[-1,]
  body <- readr::type_convert(body, )

  date <- header["Experiment Run End Time"]
  date <- date |>
    strsplit(" ")
  date <- date[[1]][1]
  date <- lubridate::ymd(date)

  experiment_type <- header["Experiment Type"]

  wells <- header["Block Type"] |>
    stringr::str_extract("[^[:space:]|\b]*(?=-Well)") |>
    as.integer()

  new_pcr(
    data = body,
    raw_data = readr::read_file_raw(path),
    header = header,
    footer = footer,
    date = date,
    wells = wells,
    experiment_type = experiment_type,
    is_tidy = FALSE
  )
}

