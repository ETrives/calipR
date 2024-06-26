% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/find_borders.R
\name{subset_window}
\alias{subset_window}
\title{subset_window}
\usage{
subset_window(dt_2, x, period = c("start", "end"))
}
\arguments{
\item{dt_2}{a data table containing the entire dataset (first ouput from resolve_mult_peaks function)}

\item{x}{a data table containing the informations regarding a given peak (second output from resolve_mult_peaks)}
}
\value{
a data table object subset with only 50 frames before a given peak
}
\description{
This function is defined to be used inside an lapply function. It subsets 50 frames
before a peak and stores it in a new data table. For each peak, there will be a corresponding
data table with only the 50 frames before the peak.
}
