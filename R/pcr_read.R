read_pcr <- function(path, pad_zero = FALSE, usr_standards = NULL) {

  # .name_repair argument silences the renaming business
  raw_data <- readxl::read_excel(path = path, sheet = "Results", col_names = FALSE,
                               .name_repair = ~ vctrs::vec_as_names(..., repair = "unique", quiet = TRUE))

  breaks <- which(is.na(raw_data[,1]))
  stopifnot(length(breaks) %in% 1:2)

  header <- raw_data |>
    dplyr::slice_head(n = (breaks[1] - 1)) |>
    dplyr::select(1:2) |>
    dplyr::rename(name = ...1,
                  value = ...2)

  if (length(breaks) == 2) {
    body <- dplyr::slice(raw_data, (breaks[1]+1):(breaks[2]-1))
    footer <- raw_data |>
      dplyr::slice((breaks[2]+1):nrow(raw_data)) |>
      dplyr::select(1:2) |>
      dplyr::rename(name = ...1,
                    value = ...2)
  }

  # These files don't always have footers
  if (length(breaks) == 1) {
    body <- dplyr::slice(raw_data, (breaks[1]+1):nrow(raw_data))
    footer <- dplyr::tibble()
  }

  # Turn the first row into colnames, then remove that row. Convert cols into
  # their proper types
  colnames(body) <- body[1,]
  body <- body[-1,]
  body <- readr::type_convert(body, )

  date <- header[which(header$name == "Experiment Run End Time"), 2][[1]]
  date <- date |>
    strsplit(" ")
  date <- date[[1]][1]
  date <- lubridate::ymd(date)

  experiment_type <- header[which(header$name == "Experiment Type"), 2][[1]]

  new_pcr(data = body, raw_data = raw_data, header = header, footer = footer,
          date = date, experiment_type = experiment_type, is_tidy = FALSE)
}
