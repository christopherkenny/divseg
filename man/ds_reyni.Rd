% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/reyni.R
\name{ds_reyni}
\alias{ds_reyni}
\alias{reyni}
\title{Compute Reyni Entropy}
\usage{
ds_reyni(.data, .cols, .name, q = 0)

reyni(..., .data = dplyr::across(everything()))
}
\arguments{
\item{.data}{\link[tibble:tibble-package]{tibble}}

\item{.cols}{\href{https://tidyselect.r-lib.org/reference/language.html}{\code{tidy-select}}
Columns to compute the measure with.}

\item{.name}{name for column with Reyni entropy. Leave missing to return a vector.}

\item{q}{exponent parameter. Default 0. Can not be 1.}

\item{...}{arguments to forward to ds_reyni from reyni}
}
\value{
a \link[tibble:tibble-package]{tibble} or numeric vector if .name missing
}
\description{
Compute Reyni Entropy
}
\examples{
data('de_county')
ds_reyni(de_county, starts_with('pop_'))
ds_reyni(de_county, starts_with('pop_'), 'reyni')
}
\concept{div}
