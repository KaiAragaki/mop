#' Constructor for a Incucyte object
#'
#' @param data `data.frame`, no restrictions on form.
#' @param raw_data `data.frame`, no restrictions on form. Typically represents
#'   read-in data with no changes to it.
#' @param date `lubridate::Date` object
#' @param is_tidy `logical`. Is the `data.frame` provided tidy?
#'
#' @return a `nanodrop` object
new_incucyte <- function(data = data.frame(), raw_data = data.frame(), date = lubridate::Date(),
                         nucleotide = character(), is_tidy = logical()) {

  stopifnot(is.data.frame(data),
            is.data.frame(raw_data))
  vec_assert(date, lubridate::Date())
  vec_assert(nucleotide, character())
  vec_assert(is_tidy, logical())

  new_vctr(list(data = I(data), raw_data = I(raw_data), date = date, nucleotide = nucleotide, is_tidy = is_tidy),
           class = "nanodrop")
}

#' Make a nanodrop object
#'
#' @param data a `data.frame` - or something coercible to one - containing
#'   nanodrop data. See details for requirements.
#' @param date an optional character vector or something coercible to a `Date`
#'   object with `lubridate::as_date`
#' @param nucleotide a character vector to specifying the analyte in the
#'   nanodrop experiment. Must match one of the following: RNA, DNA, dsDNA, or
#'   dsRNA.
#' @param is_tidy logical. Is the supplied data tidy?
#'
#' @details Making a nanodrop object 'by hand' (that is, not using
#'   `read_nanodrop`) is not recommended, since it is challenging to ensure a
#'   given `data.frame` is truly a valid nanodrop file. To increase reliability,
#'   flexibility is reduced. As such, the supplied `data.frame` must *at least*
#'   have the following columns *if* `is_tidy = TRUE`:
#'
#' * `date`
#' * `sample_name`
#' * `conc`
#' * `a260_280`
#' * `a230_280`
#'
#' If `is_tidy = FALSE`, *at least* the following columns must be provided:
#'
#' * `Date`
#' * `Sample.Name`
#' * `Nucleic.Acid.ng.uL.`
#' * `A260.A280`
#' * `A260.A230`
#'
#' Note: technically, the given column names will be stripped of all
#' non-alphanumerics and forced `tolower`, then compared against the following:
#'
#' * `date`
#' * `samplename`
#' * `nucleicacidngul`
#' * `a260a280`
#' * `a260a230`
#'
#' In both cases, other columns allowed are those that appear in Example A and
#' Example B below.
#'
#' If there are additional columns provided, they will be silently dropped.
#'
#' @return an `incucyte` object
#' @export

incucyte <- function(data, platemap) {

  raw_data <- dplyr::tibble()
  if (!is_tidy) {
    raw_data <- data
  }

  new_nanodrop(data = data, raw_data = raw_data, date = date, nucleotide = nucleotide, is_tidy = is_tidy) |>
    validate_nanodrop()
}

validate_nanodrop <- function(x) {

  # Each argument should have at most one value
  validate_nanodrop_one_arg_one_val(x)

  # Names of data, raw data include required names and not have names other than
  # permitted names. The permitted/required names depend on the value of is_tidy
  validate_nanodrop_names(x)

  x
}

validate_nanodrop_one_arg_one_val <- function(x) {
  meta <- list(date = x$date, nucleotide = x$nucleotide, is_tidy = x$is_tidy)

  if(any(lengths(meta) > 1)) {

    gr_one <- lengths(meta)[which(lengths(meta) > 1)]
    first_culprit <- names(gr_one)[1]
    first_culprit_amt <- gr_one[[1]]

    rlang::abort(c("All arguments must have only one value.",
                   x = paste("Argument", first_culprit, "has length", first_culprit_amt)))
  }
}

validate_nanodrop_names <- function(x) {

  universal_req_names <- data.frame(name = c("date", "sample_name"))

  universal_allowed_names <-
    rbind(universal_req_names,
          data.frame(
            name = c("a260", "a280", "nucleic_acid_factor",
                     "baseline_correction_nm", "baseline_absorbance",
                     "corrected_ngul", "corrected_cv", "impurity_1",
                     "impurity_1_a260", "impurity_1_cv", "impurity_1_m_m",
                     "impurity_2", "impurity_2_a260", "impurity_2_cv",
                     "impurity_2_m_m", "impurity_3", "impurity_3_a260",
                     "impurity_3_cv", "impurity_3_m_m")
          )
    )

  tidy_req_names <- rbind(universal_req_names,
                          data.frame(name = c("conc", "a260_280", "a260_230")))

  tidy_allowed_names <- rbind(tidy_req_names, universal_allowed_names)

  untidy_req_names <-
    rbind(universal_req_names,
          data.frame(name = c("nucleicacidngul", "a260a280", "a260a230"))) |>
    dplyr::mutate(name = stringr::str_remove_all(.data$name, "_"))

  untidy_allowed_names <-
    rbind(untidy_req_names, universal_allowed_names) |>
    dplyr::mutate(name = stringr::str_remove_all(.data$name, "_"))

  og_names <- colnames(x$data)

  if (x$is_tidy) {
    req_names <- tidy_req_names
    allowed_names <- tidy_allowed_names
    names_df <- data.frame(og_name = og_names,
                           name = og_names)
  } else {
    req_names <- untidy_req_names
    allowed_names <- untidy_allowed_names
    common_names <- og_names |> stringr::str_remove_all("[^[:alnum:]]") |> tolower()
    names_df <- data.frame(og_name = og_names,
                           name = common_names)
  }

  missing_req_names <- dplyr::anti_join(req_names, names_df, by = "name")

  if (nrow(missing_req_names) > 0) {
    rlang::abort(c("Data is missing required column(s)",
                   x = paste("Data should have columns named", paste(missing_req_names$name, collapse = ", "))))
  }

  disallowed_names <- dplyr::anti_join(names_df, allowed_names, by = "name")

  if (nrow(disallowed_names) > 0) {
    rlang::warn(c("Unexpected columns are being dropped",
                  i = paste("Dropping:", paste(disallowed_names$og_name, collapse = ", "))))
    x$data <- dplyr::select(x$data, -disallowed_names$og_name)
  }

  x
}

#' @export
obj_print_data.nanodrop <- function(x, ...) {
  print(x$data)
}

#' @export
obj_print_footer.nanodrop <- function(x, ...) {
  cat(crayon::silver("# Nucelotide:"),
      dplyr::case_when(x$nucleotide == "RNA" ~ crayon::red("RNA"),
                       x$nucleotide == "DNA" ~ crayon::blue("DNA"),
                       x$nucleotide == "dsRNA" ~ crayon::white$bgRed("dsRNA"),
                       x$nucleotide == "dsDNA" ~ crayon::white$bgBlue("dsDNA"),
                       is.null(x$nucleotide) ~ crayon::silver("NULL")),
      "\n")

  cat(crayon::silver("# Is tidy:"),
      dplyr::case_when(is.null(x$is_tidy) ~ crayon::silver("NULL"),
                       x$is_tidy == TRUE ~ crayon::blue(x$is_tidy),
                       x$is_tidy == FALSE ~ crayon::red(x$is_tidy)),
      "\n")

  cat(crayon::silver("# Date:"),
      ifelse(!is.null(x$date), as.character(x$date), "NULL"),
      "\n")
}

#' Test if an object is a incucyte object
#'
#' @param incucyte Object to be tested
#'
#' @return logical
#' @export
is_incucyte <- function(incucyte) {
  inherits(incucyte, "incucyte")
}
