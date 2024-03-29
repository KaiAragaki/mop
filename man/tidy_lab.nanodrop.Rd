% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/nanodrop_tidy.R
\name{tidy_lab.nanodrop}
\alias{tidy_lab.nanodrop}
\title{Tidy a nanodrop object}
\usage{
\method{tidy_lab}{nanodrop}(x, force_tidy = FALSE, ...)
}
\arguments{
\item{x}{A \code{nanodrop} object to tidy}

\item{force_tidy}{\code{logical}. Should the tidying take place, even if the
\code{is_tidy} attribute is \code{TRUE}?}

\item{...}{Unused}
}
\value{
a \code{nanodrop} object
}
\description{
Tidy a nanodrop object
}
\details{
This function:
\itemize{
\item renames columns to sensible substitutes
\item converts date-times to ISO 8601-esque date-time format (YYYY-MM-DD
HH:MM:SS vs YYYY-MM-DDTHH:MM:SSZ)
}

It's recommended that you do any manipulations to these data after you
tidy, rather than before, as \code{tidy_nanodrop} expects the output to be
fairly similar to the output from \code{read_nanodrop}.

A tidy \code{nanodrop} object will (usually) contain the following columns:
\itemize{
\item \code{date} The date-time of sample reading, as YYYY-MM-DD HH:MM:SS
\item \code{sample_name} The name of the sample provided by the NanoDrop itself
\item \code{conc} The concentration of nucleic acid in ng/uL
\item \code{a260_280} The absorbance at 260nm / absorbance at 280nm. Typically a
marker of protein contamination. Pure nucleic acid is typically around 2.
\item \code{a260_230} The absorbance at 260nm / absorbance at 230nm. Typically a
marker of guanadine salt contamination. Pure nucleic acid is typically
around 2.
\item \code{a260} The absorbance at 260nm, the wavelength nucleic acids absorb most
strongly.
\item \code{a280} The absorbance at 280nm, the wavelength proteins absorb most
strongly.
}

The remaining columns are typically unused.
}
