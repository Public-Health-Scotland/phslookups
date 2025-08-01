% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/get_hscp_locality.R
\name{get_hscp_locality}
\alias{get_hscp_locality}
\title{Get HSCP Locality lookup}
\usage{
get_hscp_locality(version = "latest", col_select = NULL)
}
\arguments{
\item{version}{A string defining a version to read in. The default value
is "latest", otherwise supply a date (file name suffix), e.g. "20230804".}

\item{col_select}{Columns to include in the results. You can use the same
mini-language as \code{dplyr::select()} to refer to the columns by name. Use
\code{c()} to use more than one selection expression. Although this
usage is less common, \code{col_select} also accepts a numeric column index. See
\code{\link[tidyselect:language]{?tidyselect::language}} for full details on the
selection language.}
}
\value{
a \link[tibble:tibble-package]{tibble} of the HSCP localities lookup
}
\description{
Read a Health and Social Care Partnership (HSCP) Locality lookup file from
cl-out into a tibble.
}
\examples{
get_hscp_locality()
get_hscp_locality(version = "20240308")
get_hscp_locality(col_select = c("datazone2011", "hscp_locality"))
}
