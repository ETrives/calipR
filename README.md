# calipR

[![Project Status: Active](https://www.repostatus.org/badges/latest/active.svg)](https://www.repostatus.org/#active) [![Licence](https://img.shields.io/badge/licence-GPL--3-blue.svg)](https://www.gnu.org/licenses/gpl-3.0.en.html) [![CRAN_Status_Badge](https://www.r-pkg.org/badges/version/badgecreatr)](https://cran.r-project.org/package=calipR) [![packageversion](https://img.shields.io/badge/Package%20version-1.0.0_alpha-orange.svg?style=round-square)](commits/develop)

A **Cal**cium **I**maging Data Analysis **P**ipeline in **R**

calipR is a free and open source software dedicated to calcium imaging data analysis. It is fully coded in R and is distributed both as an R package and as a standalone windows application.

## Key Features

-   No code workflow (Graphical User Interface (GUI))

-   Interactive parameters optimization module

-   Background estimation and signal denoising

    -   Implementation of the Derivative Passing Accumulation method [(Liu & Lin, 2019)](https://doi.org/10.1186/s12859-019-3188-4)

    -   pattern matching based new algorithm

-   Normalization

-   Signal Deconvolution [(Jewell et al. 2020)](https://doi.org/10.1093/biostatistics/kxy083) [github](https://github.com/jewellsean/FastLZeroSpikeInference)

-   Basic Statistics

-   \< in development \> Clustering Analysis [(dtwclust package)](https://github.com/asardaes/dtwclust)

## Project Goals

-   allowing non programmers to **easily** **access** and **use** a semi-automated calcium imaging data analysis pipeline without any programming knowledge/skills.

    -   *Save time*

    -   *increase reproducibility*

-   providing R users/developers a **foundation** that can be further **adapted to their specific needs**, taking advantage of R richness in terms of statistical packages and community.

## Important Notes

-   The R package includes a function which starts the same GUI as in the windows application. So MacOS and linux users can benefit from it.

-   To date, calipR doesn't include cell detection. You firstly need to process your video files in Fiji (see tutorial) to detect the cells. calipR takes csv files as input, coming from manual cell detection in Fiji OR automatic cell detection through the trackmate plugin, allowing the use of several segmentation algorithms.

## Windows Application Installation

To install the Graphical User Interface (GUI) based version for windows, you only need to download and execute the last version of the calipR.exe file available [in the release section](https://github.com/ETrives/calipR/releases). Once installed, just run the program and start your data analysis !

## R package Installation

You can download the last version from github with devtools

``` r
# Install devtools if necessary
if (!require("devtools")) install.packages("devtools")

#load devtools
library(devtools)

#install calipR from github
install_github("ETrives/calipR")
```
