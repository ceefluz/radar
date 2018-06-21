# `RadaR` ![logo_radar](man/figures/radar.png)
RadaR is an application for intuitive, rapid and reproducible quality of care pattern analysis of infectious patients.
<br>
<br>
Preprint about RadaR available: https://doi.org/10.1101/347534
<br>
<br>
See a running example [here](https://ceefluz.shinyapps.io/radar/).

## Prerequisites for using RadaR
RadaR was built in [R](https://www.r-project.org) , an open source programming language using the [Shiny package](https://shiny.rstudio.com), a web application framework for R. Users will need to download [R](https://cran.uni-muenster.de/) in order to use RadaR and we suggest the use of [RStudio](https://www.rstudio.com). R is completely free to use. All required code can be found in this github repositroy.

## Input type for RadaR's calculation
RadaR works with standard csv-files (.csv). The variables needed for RadaR are as follows:

![](man/figures/variables.png)

Usually different data sources need to be merged for the desired result (at our institution three different sources: general data warehouse, pharmacy data, microbiology data). For an easy and rapid creating process of the needed datasets that can be loaded into RadaR, an additional R-package will soon be available here in this github repository.

## Privacy and storage
RadaR works with sensitive hospital data and is based on single observations on the patient level. All data for the running [example](https://ceefluz.shinyapps.io/radar/) is simulated and don't represent any real patients. 
<br>
RadaR can be run locally on protected servers within institutions (for example: [Shiny server](https://www.rstudio.com/products/shiny/shiny-server/)) or on personal machines (mac, windows, linus).

## Author

![logo_uni](man/figures/logo_en.png)![logo_umcg](man/figures/logo_umcg.png)

RadaR was created at the Faculty of Medical Sciences of the [University of Groningen](https://www.rug.nl/) and the Medical Microbiology & Infection Prevention department of the University Medical Center Groningen (UMCG) by [Christian Luz](https://www.rug.nl/staff/c.f.luz/), PhD Student.

## Copyright
[![License](https://img.shields.io/badge/Licence-GPL%20v2.0-orange.svg)](https://github.com/ceefluz/radar/blob/master/LICENSE)
RadaR is licensed under the [GNU General Public License (GPL) v2.0](https://github.com/ceefluz/radar/blob/master/LICENSE). In a nutshell, this means that this package:

- May be used for commercial purposes

- May be used for private purposes

- May be modified, although:

  - Modifications **must** be released under the same license when distributing the package
  - Changes made to the code **must** be documented

- May be distributed, although:

  - Source code **must** be made available when the package is distributed
  - A copy of the license and copyright notice **must** be included.

- Comes with a LIMITATION of liability

- Comes with NO warranty
