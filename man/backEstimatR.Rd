% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/backEstimate_pat_match.R
\name{backEstimatR}
\alias{backEstimatR}
\title{backEstimatR}
\usage{
backEstimatR(dt, patdet_out)
}
\arguments{
\item{dt}{}

\item{patdet_out}{}
}
\value{
a data table with the estimated background trace in a new column called
"background" and the detrended Mean Grey values in a column called "background detrended"
}
\description{
Estimates background fluorescence changes not related to the signal identified
by the user (through the positive bank vs negative bank provided in the
previous step (patDetectR))
}
