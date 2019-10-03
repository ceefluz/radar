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


### Input variables for **RadaR**
#### Admission data
| Variable             	| Detail                                                                           	|
|----------------------	|----------------------------------------------------------------------------------	|
| id | Patient ID or study ID |
| adm_id | Admission ID |
| gender | Gender |
| adm_start_date | Admission date (YYYY-MM-DD)  |
| adm_end_date | Discharge date (YYYY-MM-DD)   |
| death_during_adm | Death during admission (TRUE/FALSE) |
| adm_route | Origin at admission |
| sub_specialty | Sub-specialty |
| specialty | General specialty (internal medicine, surgery, other) |
| birth_date | Birth date (YYYY-MM-DD)  |

#### Antimicrobial consumption data
| Variable             	| Detail                                                                           	|
|----------------------	|----------------------------------------------------------------------------------	|
| id| Patient ID or study ID  |
| ab_start_date| Start of antimicrobial treatment (YYYY-MM-DD) |
| ab_stop_date| Stop of antimicrobial treatment (YYYY-MM-DD) |
| ab_route | Administration route (e.g. IV, oral, ...) |
| atc_code| ATC code according WHO ATC classification system |
| ddd_per_day| Defined daily dose of antimicrobial according to WHO ATC classification system per day|

#### Microbiological data
| Variable             	| Detail                                                                           	|
|----------------------	|----------------------------------------------------------------------------------	|
| id | Patient ID or study ID |
| specialty | Ordering specialty |
| antimicrobial susceptibility testing | Several columns of tested antimicrobial agents (e.g. amox, cipr etc.) with resistance results (R / I / S) |
| mo | Microbial ID (if test is positive) following the nomenclature of the Integrated Taxonomic Information System
| material | Test material (currently supported: blood and urine) |
| test_date  | Test date (YYYY-MM-DD) |
| test_number | Test number |

These data will be loaded, merged, and transformed for analysis upon start of RadaR

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
