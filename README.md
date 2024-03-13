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

## Research using calipR

-   Meunier, M. A., Porte, C., Vacher, H., Trives, E., Nakahara, T. S., Trouillet, A. C., ... & Keller, M. (2024). Hair from Sexually Active Bucks Strongly Activates Olfactory Sensory Inputs But Fails to Trigger Early First Ovulation in Prepubescent Does. Physiology & Behavior, 275, 114451. <https://doi.org/10.1016/j.physbeh.2023.114451>

-   Poissenot, K., Trouillet, A. C., Trives, E., Moussu, C., Chesneau, D., Meunier, M., ... & Keller, M. (2023). Sexual discrimination and attraction through scents in the water vole, Arvicola terrestris. Journal of Comparative Physiology A, 1-11. <https://doi.org/10.1007/s00359-023-01671-5>

-   Nakahara, T.S., Goterris-Cerisuelo, R., Trives, E., Fuentes-Ballesteros, J.F., Sanchez-Catalan, F. M. G., Chamero, P. (2023). Sensory detection of volatile pup derived molecules by the mouse vomeronasal organ. In Chemical Senses (Vol48). GREAT CLARENDON ST, OXFORD OX2 6DP, ENGLAND: OXFORD UNIV PRESS. Abstract id (p.48): [bjad041.119](https://watermark.silverchair.com/bjad041.pdf?token=AQECAHi208BE49Ooan9kkhW_Ercy7Dm3ZL_9Cf3qfKAc485ysgAAA4YwggOCBgkqhkiG9w0BBwagggNzMIIDbwIBADCCA2gGCSqGSIb3DQEHATAeBglghkgBZQMEAS4wEQQMG2bebSMUCzGkq-ZSAgEQgIIDOdZINz5QJ9vhcpwSPterfzSo7H_6KRfft6Ek5SM-tVlTCVgFEia4xtNwKqqb6Ktyn2hy5gLergpz212_N4vkdcrjpobgQhWvp_yRP5yDAft0AwkQhpfaYVEg_fw329SsrYM1Fey-PB0KPk1hlfNdaB9K7rixQJbFfG41iBn8vVdsuUy5zH9POXbVVCx8EUyFS9Y-lNnJ1Wo6FY9N6qfzhx-atDXRManSokRIieOkZ2LlVC1-4jVqDdOA8Fw24jkB7QVSU7jhesnt3Z7Js_19AjSBvK8OP7P75Z3LuPTl678SWddwyDtS3go2hls3hMRjlKBAXgsso-ctVe6QLVPIeiV--N9id8OKHug-KkvA3vQvDqvBxyDshKfLbXr216MegAaNEtnCB8s_dlf_6fn3G-cwfjKbGT7Zu4BVlORZrdu0HxjQoVSu1aHDKQWpQsAmGyk45S6ZXNjRf3WQVah5chHFNnvx7s9OLwJ7NyePNGCxHhB4CZmEgU3sh5uZopyDzwf6HSWo_ac4U3JMjwml4tsu3ET-_yG1lb6uBaPRJ15eZ2Cr4rs2HhTPtt1UnkZgJ6WXvCOb0MuJZ4k42Kl4UyF7hgh2JdHPsWNRv53sL19KtpqNtxWs94sk2POfBne_uTQ7uQ9wGAhZveOahw-PLPSGSvFsIgvWPh-LcLg3Ile-jbF4jARk6WjgwrbSjbCjJfJ8uYtJuSQjS8jUcgIkEtX8X353aCTYSzixOYv96Jg4TQSNS6mvSL0K9sYi4nvfPz_GchIGVCS7JcsGrGaAOz0noyoFziCcFosKt3MzdCUPLtZQbW00o_SuTBbuAzwHkcbRtuHobPfMZGYaykQ1aIChH6DmVSjNJq0RQsesUc7hITJEfuUDfLbdOcd0iZrkSkCREOXOe9ZS5PSs332g2lQOcAMPtcOv5zCYUGQMZzItCLEUsSz5GxcZ6KbwdU7yYMnIwKQYCNNAP_AqO3e2lv3vxy0S7oWvF7TDSdqO91ItVH6N02D-hmPaipoaXG16qXv7G3tjOntDn4CPTj0JZL-sK60aaB2tFJ9tXPLKAf9COrBIPylmaxYbIlg5oQDSWjlIUCtM3KNlHA)

## Get Started

Check the full documentation at [https://etrives.github.io/calipR/](https://etrives.github.io/calipR/)


