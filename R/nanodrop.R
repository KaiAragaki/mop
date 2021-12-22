#' Constructor for a nanodrop object
#'
#' As this is a low-level constructor, it will let you do some un-recommended
#' things without warning. You have been warned.
#'
#' @param data `data.frame`, no restrictions on form.
#' @param raw_data `data.frame`, no restrictions on form. Typically represents
#'   read-in data with no changes to it.
#' @param date `lubridate::Date` object
#' @param nucleotide `character`. Should likely be DNA, RNA, dsDNA, or even
#'   dsRNA.
#' @param is_tidy `logical`. Is the `data.frame` provided tidy?
#'
#' @return a `nanodrop` object
new_nanodrop <- function(data = data.frame(), raw_data = data.frame(), date = lubridate::Date(),
                         nucleotide = character(), is_tidy = logical()) {

  stopifnot(is.data.frame(data),
            is.data.frame(raw_data))
  vec_assert(date, lubridate::Date())
  vec_assert(nucleotide, character())
  vec_assert(is_tidy, logical())

  meta <- list(date = date, nucleotide = nucleotide, is_tidy = is_tidy)

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
#' @return a `nanodrop` object
#' @export
#'
#' @examples
#'
#' # Example A: colnames allowed when is_tidy = FALSE
#' a <- system.file("extdata", "nanodrop.csv", package = "ragaki") |>
#' read_nanodrop()
#'
#' colnames(a$data)
#'
#' # Technically, these are the names that are checked for after the given names
#' # have alphanumerics removed and are converted to lowercase:
#'
#' colnames(a$data) |> stringr::str_remove_all("[^[:alnum:]]") |> tolower()
#'
#'
#' # Example B: colnames allowed when is_tidy = TRUE
#' b <- a |> tidy_lab()
#'
#' colnames(b$data)
#'
nanodrop <- function(data,
                     date = NULL,
                     nucleotide = c("RNA", "DNA", "dsDNA", "dsRNA"),
                     is_tidy = FALSE) {

  stopifnot(is.logical(is_tidy))

  if (missing(nucleotide) | is.null(nucleotide)) {
    nucleotide <- character(length = 1L)
  } else {
    nucleotide <- rlang::arg_match(nucleotide)
  }

  if (missing(date) | is.null(date)) {
    date <- lubridate::Date(length = 1L)
  } else {
    date <- lubridate::as_date(date)
  }

  data <- dplyr::as_tibble(data)

  raw_data <- dplyr::tibble()
  if (!is_tidy) {
    raw_data <- data
  }

  new_nanodrop(data = data, raw_data = raw_data, date = date, nucleotide = nucleotide, is_tidy = is_tidy) |>
    validate_nanodrop()
}

validate_nanodrop <- function(nanodrop) {

  lengths <- lapply(nanodrop, length)
  names(lengths) <- names(nanodrop)
  gr_one <- nanodrop[which(lengths > 1)]

  if(length(gr_one) > 0 & (!names(gr_one)[1] %in% c("data", "raw-data"))) {
    culprit <- names(gr_one)[1]
    culprit_amt <- lengths[[culprit]]

    stop("All arguments must have only one value, but ", culprit, " has ", culprit_amt)
  }

  # Validate names

  req_names_tidy <- c(
    "date", "sample_name", "conc", "a260_280", "a260_230"
  )

  allowed_names_tidy <- c(
    "date", "sample_name", "conc", "a260_280", "a260_230", "a260",
    "a280", "nucleic_acid_factor", "baseline_correction_nm", "baseline_absorbance",
    "corregted_ngul", "corrected_cv", "impurity_1", "impurity_1_a260",
    "impurity_1_cv", "impurity_1_m_m", "impurity_2", "impurity_2_a260",
    "impurity_2_cv", "impurity_2_m_m", "impurity_3", "impurity_3_a260",
    "impurity_3_cv", "impurity_3_m_m", "tube_name", "cell_line", "experimental_condition"
  )

  req_names_untidy <- data.frame(common_names = c(
    "date", "samplename", "nucleicacidngul", "a260a280", "a260a230"
  ))

  allowed_names_untidy <- data.frame(common_names = c(
    "date", "samplename", "nucleicacidngul", "a260a280", "a260a230", "a260",
    "a280", "nucleicacidfactor", "baselinecorrectionnm", "baselineabsorbance",
    "correctedngul", "correctedcv", "impurity1", "impurity1a260",
    "impurity1cv", "impurity1mm", "impurity2", "impurity2a260",
    "impurity2cv", "impurity2mm", "impurity3", "impurity3a260",
    "impurity3cv", "impurity3mm", "tubename", "cellline", "experimentalcondition"
  ))

  if (nanodrop$is_tidy) {

    missing_req_names <- setdiff(req_names_tidy, colnames(nanodrop$data))

    if (length(missing_req_names) > 0) {
      rlang::abort(c("Data is missing required column(s)",
                     i = paste("Data should have columns named", paste(missing_req_names, collapse = ", "))))
    }

    nanodrop$data <- dplyr::select(nanodrop$data, dplyr::matches(allowed_names_tidy))

  } else {

    common_names <- colnames(nanodrop$data) |> stringr::str_remove_all("[^[:alnum:]]") |> tolower()

    names_df <- data.frame(og_names = colnames(nanodrop$data), common_names = common_names)

    req_names_untidy <- data.frame(common_names = req_names_untidy)

    missing_req_names <- dplyr::anti_join(req_names_untidy, names_df, by = "common_names")

    if (nrow(missing_req_names) > 0) {
      stop("data should have a column named ", paste(missing_req_names$common_names, collapse = ", "))
    }

    allowed <- dplyr::semi_join(names_df, allowed_names_untidy, by = "common_names")

    nanodrop$data <- dplyr::select(nanodrop$data, dplyr::matches(allowed$og_names))

  }


  nanodrop
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

#' Coerce object to nanodrop
#'
#' @param x an object to coerce
#' @param ... additional arguments passed to their respective functions
#'
#' @return a `nanodrop` object
#' @export
as_nanodrop <- function(x, ...) {
  UseMethod("as_nanodrop")
}

#' @param x a `data.frame`
#' @param nucleotide an optional character vector describing the measured nucleotide (DNA, RNA, dsDNA, dsRNA)
#' @param is_tidy Logical. Is `data` already tidy?
#' @param date Character coercible to a `Date` object via `lubridate::as_date`
#'
#' @rdname as_nanodrop
#' @export
as_nanodrop.data.frame <- function(x, nucleotide = NULL, is_tidy = FALSE, date = NULL, ...) {


  nucleotide <- if

  date <- lubridate::as_date(date, )

  date <- if (is.null(date)) lubridate::Date() else date



  x <- dplyr::as_tibble(x)

  raw_data <- if (is_tidy) data.frame() else x

  new_nanodrop(data = x, raw_data = raw_data, nucleotide = nucleotide, is_tidy = is_tidy, date = date) |>
    validate_nanodrop()
}

#' Test if an object is a nanodrop object
#'
#' @param nanodrop Object to be tested
#'
#' @return logical
#' @export
is_nanodrop <- function(nanodrop) {
  inherits(nanodrop, "nanodrop")
}
