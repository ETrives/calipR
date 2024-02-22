# calipR

[![Project Status: Active](https://www.repostatus.org/badges/latest/active.svg)](https://www.repostatus.org/#active) [![Licence](https://img.shields.io/badge/licence-GPL--3-blue.svg)](https://www.gnu.org/licenses/gpl-3.0.en.html) [![CRAN_Status_Badge](https://www.r-pkg.org/badges/version/badgecreatr)](https://cran.r-project.org/package=calipR) [![packageversion](https://img.shields.io/badge/Package%20version-1.0.0_alpha-orange.svg?style=round-square)](commits/develop)

A **Cal**cium **I**maging Data Analysis **P**ipeline in **R**

calipR is a free and open source software dedicated to calcium imaging data analysis. It is fully coded in R and is distributed both as an R package and as a standalone windows application.\

## Key Features

-   No code workflow (Graphical User Interface (GUI))

-   Interactive parameters optimization module

-   Background estimation and signal denoising

    -   Implementation of the Derivative Passing Accumulation method [(Liu & Lin, 2019)](https://doi.org/10.1186/s12859-019-3188-4)

    -   pattern matching based new algorithm

-   Normalization

-   Signal Deconvolution [(Jewell et al. 2020)](https://doi.org/10.1093/biostatistics/kxy083) [github](https://github.com/jewellsean/FastLZeroSpikeInference)

-   Basic Statistics

-   

    > in development - Clustering Analysis [(dtwclust package)](https://github.com/asardaes/dtwclust)

## Project Goals

-   allowing non programmers to **easily** **access** and **use** a semi-automated calcium imaging data analysis pipeline without any programming knowledge/skills.

    -   *Save time*

    -   *increase reproducibility*

-   providing R users/developers a **foundation** that can be further **adapted to their specific needs**, taking advantage of R richness in terms of statistical packages and community.

## Important Notes

-   The R package includes a function which starts the same GUI as in the windows application. So MacOS and linux users can benefit from it.

-   To date, calipR doesn't include cell detection. You firstly need to process your video files in Fiji (see tutorial) to detect the cells. calipR takes csv files as input, coming from manual cell detection in Fiji OR automatic cell detection through the trackmate plugin, allowing the use of several segmentation algorithms.

## Windows Application Installation

To install the Graphical User Interface (GUI) based version for windows, you only need to download and execute the last version of the calipR.exe file available in the [release section](https://github.com/ETrives/calipR/releases). Once installed, just run the program and start your data analysis!

## R package Installation

You can download the latest version from r-universe

``` r

install.packages('calipR', repos = c('https://etrives.r-universe.dev', 'https://cloud.r-project.org'))
```

or from github with devtools

``` r
# Install devtools if necessary
if (!require("devtools")) install.packages("devtools")

#load devtools
library(devtools)

#install calipR from github
install_github("ETrives/calipR")
```

if you want to install a specific version

``` r
# Install devtools if necessary
if (!require("devtools")) install.packages("devtools")

#load devtools
library(devtools)

#Install calipR version v1.0.0-alpha
install_github("ETrives/calipR@v1.0.0-alpha")
```

### Troubleshooting Installation

#### MacOS

##### You lack a software to build packages with compiled code

During calipR installation, a window should open and say you don't have this software and ask you if you want to install it. Say Yes ! it is necessary. After being installed, launch again calipR installation through devtools or r-universe.

if the window doesn't show up or if you want to install it by your self, the software you need is Xcode Command Line Tools. To install it, open your command terminal, type the following and press enter :

``` bash
xcode-select --install
```

##### Installation successfull but GUI not working : X11 library is missing : install xquartz

You may succeed the installation step but encounter this error when trying to use calipR's graphical user interface. On macOS you need to install xquartz from [here](https://www.xquartz.org/) after downloading and installing XQuartz, the application should work correctly.

#### Windows

##### You lack a software to build packages with compiled code

if you use Rstudio, during calipR installation, a window will open and tell you don't have this software and ask you if you want to install it. Say Yes ! it is necessary. After being installed, launch again calipR installation through devtools or r-universe.

if you don't use Rstudio or want to install it by your self, [RTools](https://cran.r-project.org/bin/windows/Rtools/) is the software you need if you work on a PC. Be careful, you need to install an RTools version which matches your R version. So if you have R 4.3, you should install RTools 4.3

### OS independent :

If you encounter package dependency issues and you want to go for a radical option you can either :

#### update your R packages

``` r
update.packages(ask = FALSE, checkBuilt = TRUE)
```

#### Delete R and the associated packages and download a fresh R [this might help](https://www.ozturkibrahim.com/how-to-uninstall-r-and-rstudio-with-all-packages-settings-and-everything-else-on-windows/)

``` r
# In your fresh R session install devtools
install.packages("devtools")

# Then install calipR from github
install_github("ETrives/calipR")
    
# Or from r-universe
install.packages('calipR', repos = c('https://etrives.r-universe.dev', 'https://cloud.r-project.org'))
```

## Data Analysis Tutorial

### Preparing a correct Input for calipR

calipR v1.0.0 starts the analysis workflow after the cell detection step. So you first need to do cell detection by yourself and end up with a csv file that can be of two different types :

-   wide format : each column contains the fluorescence values for one cell across a given recording session. The corresponding data frame has the           dimensions : frame number\*cell number. This is the output you get if you follow the manual annotation tutorial in ImageJ here.
-   long format : The different cells detected are in lines and identified by a column named TRACK_ID and the corresponding fluorescence values are in       another column named MEAN_INTENSITY_CH1. The data frame dimensions are thus (cell number*frame number)*2

#### Manual Annotation Tutorial (very tedious)

#### Trackmate Annotation Tutorial (automatic)

If you don't have it, download Fiji [from here](https://fiji.sc/), it already contains the trackmate plugin.
