% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/spectramax_read.R
\name{read_spectramax}
\alias{read_spectramax}
\title{Read and tidy a SPECTRAmax file}
\usage{
read_spectramax(path, date = Sys.Date(), wavelengths = NULL)
}
\arguments{
\item{path}{Path to the SPECTRAmax .txt file}

\item{date}{Date of experiment. If not supplied, uses current date.}

\item{wavelengths}{What wavelengths were read in this experiment?}
}
\value{
A list, with the following form: \describe{
\item{data}{a \code{tibble} that contains two columns - the \code{type}, and the (tidy)
\code{data} itself in a list-column.}
\item{date}{character. The date of the experiment}
\item{experiment_type}{character. The type of experiment (currently only pq
(Protein Quantification) and mtt supported)}
\item{tidy}{logical. Should always return TRUE if read in using \code{read_spectramax}}
}
}
\description{
Read and tidy a SPECTRAmax file
}
\details{
SPECTRAmax files cannot be read in easily without tidying them
simultaneously, due to their non-rectangular structure. Therefore, tidying
is \emph{required} to be read in and is not an option supplied.
}
\examples{
system.file("extdata", "spectramax.txt", package = "mop") |>
  read_spectramax()
}
