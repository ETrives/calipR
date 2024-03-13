# calipR

[![Project Status: Active](https://www.repostatus.org/badges/latest/active.svg)](https://www.repostatus.org/#active) [![Licence](https://img.shields.io/badge/licence-GPL--3-blue.svg)](https://www.gnu.org/licenses/gpl-3.0.en.html) [![CRAN_Status_Badge](https://www.r-pkg.org/badges/version/badgecreatr)](https://cran.r-project.org/package=calipR) [![packageversion](https://img.shields.io/badge/Package%20version-1.0.0_alpha-orange.svg?style=round-square)](commits/develop)

A **Cal**cium **I**maging Data Analysis **P**ipeline in **R**

calipR is a free and open source software dedicated to calcium imaging data analysis. It is fully coded in R and is distributed as an R package, a standalone windows application, a docker image.

![Demo](https://github.com/ETrives/calipR/blob/master/Demo_calipR.gif)

## Key Features

-   No code workflow (Graphical User Interface (GUI))

-   Interactive parameters optimization module

-   Improved Background estimation and signal denoising

    -   Implementation of the Derivative Passing Accumulation method [(Liu & Lin, 2019)](https://doi.org/10.1186/s12859-019-3188-4)

    -   Development of a supervised machine learning classifier

-   Normalization

-   Signal Deconvolution [(Jewell et al. 2020)](https://doi.org/10.1093/biostatistics/kxy083) [github](https://github.com/jewellsean/FastLZeroSpikeInference)

-   Basic Statistics

-   > in development Clustering Analysis [(dtwclust package)](https://github.com/asardaes/dtwclust)

-   > in development Peaks metrics analysis (area, kinetics, duration...)


## Project Goals

-   allowing non programmers to **easily** **access** and **use** a semi-automated calcium imaging data analysis pipeline without any programming knowledge/skills.

    -   *Save time*

    -   *Go deeper*
    
    -   *Increase reproducibility*


-   providing R users/developers a **foundation** that can be further **adapted to their specific needs**, taking advantage of R richness in terms of statistical packages and community.

## Get Started

Check the full documentation at [https://etrives.github.io/calipR/](https://etrives.github.io/calipR/)
