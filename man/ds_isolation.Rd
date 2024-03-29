% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/isolation.R
\name{ds_isolation}
\alias{ds_isolation}
\alias{isolation}
\title{Compute Isolation Index}
\usage{
ds_isolation(.data, .cols, .name, .comp = FALSE)

isolation(..., .data = dplyr::across(everything()))
}
\arguments{
\item{.data}{\link[tibble:tibble-package]{tibble}}

\item{.cols}{\href{https://tidyselect.r-lib.org/reference/language.html}{\code{tidy-select}}
Columns to compute the measure with. Must be at least 2 columns. If more than 2, treats
first column as first group and sum of other columns as second.}

\item{.name}{name for column with Isolation index. Leave missing to return a vector.}

\item{.comp}{Default is FALSE. FALSE returns the sum, TRUE returns the components.}

\item{...}{arguments to forward to ds_isolation from isolation}
}
\value{
a \link[tibble:tibble-package]{tibble} or numeric vector if .name missing
}
\description{
Compute Isolation Index
}
\examples{
data('de_county')
ds_isolation(de_county, c(pop_white, starts_with('pop_')))
ds_isolation(de_county, starts_with('pop_'), 'isolation')
}
\concept{exposure}
