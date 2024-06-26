% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/deconvolution.R
\name{deconvolve}
\alias{deconvolve}
\title{deconvolve}
\usage{
deconvolve(
  norm_data,
  gam = 0.95,
  lambda = 1,
  var = "gam_detrended",
  peak_frame = 10
)
}
\arguments{
\item{norm_data}{a data table output from norm_df() function}

\item{gam}{a double between 0 and 1 modulating the modeled peak decay (i.e.
a gam of 0.99 models much longer decay than a gam of 0.5) default to 0.95.}

\item{lambda}{an integer controlling the sensitivity of spike detection (i.e.
a lambda of 1 will find much more spikes for a given event than a lambda of 1000).
In v.1.0.0-alpha, deconvolution is only used to find calcium events, not spikes.
So increasing this number to the point where you only find one (or few) spike(s) per calcium event
can largely increase the processing speed and will not affect the results. You can interactively optimize
this parameter in the parameter optimization tab in the GUI.}

\item{var}{variable used for deconvolution. Use the denoised trace for good
performances ("gam_detrended" if using DPA noise estimation or "background_detrended"
if using pattern matching noise estimation)}
}
\description{
Deconvolution of the denoised fluorescence trace based on the FastL0SpikeInference package.
To date, this step is used for two things : identifiying the start of calcium events and efficiently identifying
multiple peaks during a given stimulus. Found spikes are not used as is to infer spiking activity but will be in the future.
}
