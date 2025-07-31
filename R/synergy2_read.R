#' Read and tidy a Synergy2 file
#'
#' @param path Path to the Synergy2 .txt file
#'
#' @return A list, with the following form: \describe{
#' \item{data}{a `tibble` with all well-level experimental data combined}
#' \item{wavelengths}{a numeric vector containing the wavelengths read}
#' \item{metadata}{A `list` containing residual, non-well data}
#' }
#' @export
#' @examples
#' system.file("extdata", "bca-assay-all-export.txt", package = "mop") |>
#'   read_synergy2()
read_synergy2 <- function(path, date = Sys.Date()) {
  out <- read_synergy2_txt(path)
  wavelengths <- get_wavelengths(out)
  new_synergy2(
    data = out$data,
    wavelengths = wavelengths,
    metadata = out[-which(names(out) == "data")]
  )
}

read_synergy2_txt <- function(path) {
  readr::read_lines(path) |>
    get_sections() |>
    format_sections()
}

get_sections <- function(data) {
  # Strategy
  #
  # If there is a line that has only a single item, assume it is a header unless
  # it is one of the following:
  #
  # Calculation Warnings
  # Error/Warning messages
  # Field Group
  #
  # These headers are typically followed by things that are also single items,
  # or are empty, and will be skipped

  in_section <- FALSE
  sections <- list()
  section_names <- c()
  current_section <- 0

  for (line in data) {
    if (!in_section) {
      if (is_header_line(line)) {
        in_section <- TRUE
        just_below_header <- TRUE
        section_names <- c(section_names, line)
        current_section <- current_section + 1
      }
      next
    }

    if (line == "" && !just_below_header) {
      in_section <- FALSE
      next
    }

    if (line != "") {
      if (length(sections) < current_section) {
        sections <- c(sections, line)
      } else {
        sections[[length(sections)]] <- c(sections[[length(sections)]], line)
      }
    }

    just_below_header <- FALSE
  }

  names(sections) <- section_names
  sections

}

is_header_line <- function(line) {
  !stringr::str_detect("\t", line)
}

format_sections <- function(sections) {
  # Need to do it this way, rather than with a simple lapply, because we need to
  # capture the name of the section too
  section_types <- mapply(get_section_type, sections, names(sections))
  has_matrices <- any(section_types == "matrix")
  has_tables <- any(section_types == "table")
  has_skips <- any(section_types == "skip")

  formatted_sections <- mapply(format_section, sections, names(sections))

  # Fun fact: toxicity assays have a "% Toxicity" *and* "%Toxicity"
  #
  # This fixes that and makes working with columns a bit easier
  formatted_sections <- lapply(
    formatted_sections,
    \(x) {
      if (is.data.frame(x)) {
        colnames(x) <- stringr::str_remove_all(colnames(x), " ")
      }
      x
    }
  )

  # Coalesce all matrices an well tables into comprehensive data.frames
  if (has_matrices) {
    matrix_indices <- which(section_types == "matrix")
    suppressMessages({
      matrix_data <- Reduce(dplyr::inner_join, formatted_sections[matrix_indices])
    })
    formatted_sections <- formatted_sections[-matrix_indices]
    formatted_sections$matrix_data <- matrix_data
    section_types <- section_types[-matrix_indices]
    section_types <- c(section_types, matrix_data = "matrix")
  }

  if (has_tables) {
    has_well_id_col <- sapply(
      formatted_sections, \(x) "WellID" %in% colnames(x)
    )

    # don't try to coalesce freshly made table from matrix data
    is_matrix_data <- names(section_types) == "matrix_data"

    well_table_indices <- which(
      has_well_id_col & (section_types == "table") & !is_matrix_data
    )

    if (length(well_table_indices) > 0) {
      suppressMessages({
        well_table_data <- Reduce(
          dplyr::inner_join, formatted_sections[well_table_indices]
        )
      })
      formatted_sections <- formatted_sections[-well_table_indices]
      formatted_sections$well_table_data <- well_table_data
      section_types <- section_types[-well_table_indices]
    }
  }

  if (has_skips) {
    skips_indices <- which(section_types == "skip")
    formatted_sections <- formatted_sections[-skips_indices]
    section_types <- section_types[-skips_indices]
  }

  # Remove sections with 0 rows.

  # This happens sometimes if a matrix is empty - like if a layout is
  # unspecified
  empty_sections <- which(unlist(lapply(formatted_sections, \(x) nrow(x) == 0)))

  if (length(empty_sections) > 0) {
    formatted_sections <- formatted_sections[-empty_sections]
  }

  coaleced_indices <- which(
    names(formatted_sections) %in% c("matrix_data", "well_table_data")
  )

  if (length(coaleced_indices) == 2) {
    suppressMessages({
      formatted_sections$data <- dplyr::full_join(
        formatted_sections$matrix_data,
        formatted_sections$well_table_data
      )
    })
    formatted_sections <- formatted_sections[-coaleced_indices]
  } else if (length(coaleced_indices) == 1) {
    formatted_sections$data <- formatted_sections[[coaleced_indices]]
    formatted_sections <- formatted_sections[-coaleced_indices]
  }

  formatted_sections
}

format_section <- function(section, section_name) {
  type <- get_section_type(section, section_name)

  if (type == "matrix") formatted <- format_section_matrix(section)
  if (type == "table") formatted <- format_section_table(section)
  if (type == "dictionary") formatted <- format_section_dictionary(section)
  if (type == "skip") formatted <- NULL

  formatted
}

get_section_type <- function(sec, name) {

  # These are sections that shouldn't be processed - likely because I don't have
  # enough information to parse them reliably.
  skip_section_names <- c("Field Group", "Calculation Warnings")
  if (name %in% skip_section_names) return("skip")

  # As far as I can tell, all matrices share the fact that they start with tab
  # delimited numbers
  if (stringr::str_detect(sec[1], "^(\t[0-9]+)*$")) return("matrix")

  dictionary_section_names <- c("Data Reduction Summary", "Procedure Summary")
  if (name %in% dictionary_section_names) return("dictionary")

  # Anything not one of the above is presumed to be a table
  "table"
}

format_section_matrix <- function(sec) {
  # Section matrices can store many values per well. We must figure out how many
  # values are being stored per well.

  # First, get the number of 'plate rows' (as opposed to the matrix, which may
  # have more rows than the plate did due to, again, multiple values per well)
  plate_rows <- sum((stringr::str_extract(sec, "^[^\t]*") |> unique()) != "")

  plate_cols <- length(
    stringr::str_split(sec[1], "\t", simplify = TRUE) # returns "" "1" "2"...
  ) - 1 # -1 removes leading ""

  # -1 to exclude header
  values_per_well <- (length(sec) - 1) %/% plate_rows

  out <- tidyr::expand_grid(
    .row = seq_len(plate_rows), .col = seq_len(plate_cols)
  )

  # Now we unravel each interleaved matrix:
  for (i in seq_len(values_per_well)) {
    rows <- sec[
      seq(from = i + 1, # skip header
          to = length(sec),
          by = values_per_well)
    ]
    rows <- stringr::str_split(rows, "\t")
    value_name <- rows[[1]][length(rows[[1]])]


    values <- rows |>
      lapply(\(x) x[-c(1, length(x))]) |> # Trim off column name and value name
      unlist() |>
      as.data.frame()

    names(values) <- value_name

    out <- cbind(out, values)
  }

  suppressMessages({
    out <- readr::type_convert(out)
  })
  out <- tibble::as_tibble(out)

  # Some Well IDs are called things like SPL1:1 or something
  #
  # This information seems to be contained in other variables and prevents
  # joining, so we trim off the colon and everything after
  if ("Well ID" %in% colnames(out)) {
    out$`Well ID` <- stringr::str_remove(out$`Well ID`, ":[:digit:]*$")
  }

  out <- prepend_nm_to_wavelength_colnames(out)

  out
}

format_section_table <- function(sec) {
  out <- tryCatch(
    readr::read_tsv(I(sec), show_col_types = FALSE),
    # Small tables can fail to parse. This manually creates them.
    warning = \(msg) {
      splits <- stringr::str_split(sec, "\t")
      names <- splits[[1]]
      rest <- splits[-1]
      df <- data.frame(matrix(ncol = length(names)))
      names(df) <- names
      Reduce(rbind, rest, init = df)[-1, ]
    },
    error = \(msg) {
      splits <- stringr::str_split(sec, "\t")
      names <- splits[[1]]
      rest <- splits[-1]
      df <- data.frame(matrix(ncol = length(names)))
      names(df) <- names
      Reduce(rbind, rest, init = df)[-1, ]
    }
  )

  # No idea how fragile this is
  if ("Well ID" %in% colnames(out)) out <- tidyr::fill(out, "Well ID")

  if ("Well" %in% colnames(out)) {
    out$.row <- match(stringr::str_extract(out$Well, "^[A-Z]"), LETTERS)
    out$.col <- as.numeric(stringr::str_extract(out$Well, "[:digit:]*$"))
    out <- out |>
      dplyr::select(-"Well") |>
      dplyr::relocate(".row", ".col")
  }

  # We can calculate these values ourselves, and their generic names make
  # joining tables dicey. Drop them.
  calculated_cols <- c("Mean", "Std Dev", "CV (%)", "Count")
  if (any(calculated_cols %in% colnames(out))) {
    out <- out[!(colnames(out) %in% calculated_cols)]
  }

  out <- prepend_nm_to_wavelength_colnames(out)

  out
}

# Convert wavelength cols (562) to something more legal, column-name wise
# (nm562)
#
# This also aligns it more to the output of spectramax tidying
prepend_nm_to_wavelength_colnames <- function(x) {
  wavelength_cols <- stringr::str_detect(colnames(x), "^[2-9][0-9]{2}$")
  if (any(wavelength_cols)) {
    new_names <- stringr::str_replace(colnames(x), "^([2-9][0-9]{2})$", "nm\\1")
    colnames(x) <- new_names
  }
  x
}

format_section_dictionary <- function(sec) {
  sec |>
    lapply(format_one_dictionary_entry) |>
    Reduce(rbind, x = _)
}

format_one_dictionary_entry <- function(entry) {
  items <- stringr::str_split(entry, "\t")[[1]]
  name <- items[1]
  values <- stringr::str_split(items[-1], ",")[[1]] |>
    stringr::str_squish()
  data.frame(name = name, value = values)
}

# Wavelength data is also stored in Procedure Summary. If deemed necessary, this
# can be used to crossvalidate.
#
# The regex is fairly strict - should only match for columns named a number
# between 200 and 999 (the range for the synergy2) with "nm" prepended (which is
# done at time of section formatting)
get_wavelengths <- function(x) {
  x$data |>
    colnames() |>
    stringr::str_extract("(?<=^nm)[2-9]{1}[0-9]{2}$") |>
    na.omit() |>
    as.numeric()
}

# On sections and their forms:
#
# Generally, a section can come in one of three basic forms, which I'm calling
# 'matrix', 'table', and 'dictionary'.

# At their simplest, matrices look a bit like this:
#
# $`570`
# [1] "\t1\t2\t3\t4\t5\t6\t7\t8\t9\t10\t11\t12"
# [2] "A\t0.046\t0.046\t0.046\t0.046\t0.045\t0.045\t0.046\t0.046\t0.045\t0.046\t0.046\t0.097\t570"
# [3] "B\t0.048\t0.118\t0.600\t0.832\t1.283\t1.414\t0.110\t0.637\t0.709\t0.992\t1.288\t0.095\t570"
# [4] "C\t0.046\t0.115\t0.604\t0.847\t0.928\t1.322\t0.111\t0.719\t0.727\t0.956\t1.381\t0.104\t570"
# ...
#
# Sometimes the values are named, such as in the case of 'Well ID' and 'Name':
#
# $Layout
# [1] "\t1\t2\t3\t4\t5\t6\t7\t8\t9\t10\t11\t12"
# [2] "A\tSPL1\tSPL1\tSPL1\tSPL9\tSPL9\tSPL9\tSPL17\tSPL17\tSPL17\tSPL25\tSPL25\tSPL25\tWell ID"
# [3] "\t\t\t\t\t\t\t\t\t\t\t\t\tName"
# [4] "B\tSPL2\tSPL2\tSPL2\tSPL10\tSPL10\tSPL10\tSPL18\tSPL18\tSPL18\tSPL26\tSPL26\tSPL26\tWell ID"
# ...
#
# Note that Name is unpolpulated. Note also that there is no 'empty field' after
# \t12, making this matrix 'ragged' with differing numbers of fields per line.
#
# Here is another example that has three lines of data instead of two:
#
# $Layout
# [1] "\t1\t2\t3\t4\t5\t6\t7\t8\t9\t10\t11\t12"
# [2] "A\tBLK\tSTD1\tSTD2\tSTD3\tSTD4\tSTD5\tSPL1\tSPL2\tSPL3\tSPL4\tSPL5\tSPL6\tWell ID"
# [3] "\t\t0\t10\t25\t50\t100\t\t\t\t\t\t\tConc/Dil"
# [4] "\t\t\t\t\t\t\t\t\t\t\t\t\tName"
#

# 'Tables' and 'Dictionaries' differ in that while a table has column names
# followed by values...
#
# $`Well IDs`
# [1] "Well ID\tName" "SPL1\t"        "SPL2\t"        "SPL3\t"
# ...
#
# ...dictionaries are key-value pairs:
#
# $`Data Reduction Summary`
# [1] "Transformation\tBlank 562"   "Curve Analysis\tCurve, Conc"
#
# There tends to be no hard differences between the two, technically speaking.
# Tables in general are as long as there are wells in the plate, but that may
# not be assured. Dictionaries are only two columns wide, but tables can be two
# columns wide as well. They tend to include the name "Summary" but it is
# unclear if this is a stable trait
#
# Because of these similarities, I'll just use section names to pick out
# sections that are usually dictionaries.
#
# NOTE: as an aside, some tables will need 'filling downwards':
#
# Curve Fitting Details
#
# Curve Name  Curve Formula   Parameter   Value    Std. Error   95% CI min  95% CI max
# Curve       Y=A*X+B         A           0.00348  0.000173     0.00293     0.00402
#                             B           0.0143   0.00888      -0.0139     0.0425

# LIKELY TRUTHS

# A section always ends with a newline

# If a row has more than one item (tab delimited), it is not a header

# A row with only one item is not guaranteed to be a header (such as 'No event
# recorded' under Error/Warning messages)

# A header does not need to be unique

# Wavelengths present in the Procedure Summary section...

## Procedure Summary
##
## Plate Type	96 WELL PLATE
## Shake	Shake:  Medium for 0:10
## Read	Read:  570, 670

# ...have wavelengths that MAY be headers

# Procedure Summary block does not need to be present, however

# Likewise, for Data Reduction Summary:

## Data Reduction Summary
##
## Transformation	dual wavelength 570-670

# In this example, dual wavelength 570-670 is *two* headers - one that
# contains tabular data, one that contains matrix data

## Data Reduction Summary
##
## Transformation	Blank 562
## Curve Analysis	Curve, Conc

# In this example, Blank 562 is a header. Curve is not, but Conc is.
