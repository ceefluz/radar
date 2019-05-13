# LIST OF REQUIRED PACKAGES -----------------------------------------------

required_packages <- c(
  "AMR",
  "data.table",
  "DT",
  "ggridges",
  "lubridate",
  "plotly",
  "qicharts2",
  "rintrojs",
  "shiny",
  "shinyBS",
  "shinycssloaders",
  "shinydashboard",
  "shinyjs",
  "shinyWidgets",
  "survival",
  "survminer",
  "tidyverse",
  "viridis",
  "zoo"
)

# install missing packages

new.packages <- required_packages[!(required_packages %in% installed.packages()[,"Package"])]

if (length(new.packages)) {
  install.packages(new.packages)
}