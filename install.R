source('dependencies.R')

# install missing packages
new.packages <- required_packages[!(required_packages %in% installed.packages()[,"Package"])]
if (length(new.packages)) {
  install.packages(new.packages)
}
