

# WELCOME TO RadaR

#RadaR is licensed under the GNU General Public License (GPL) v2.0 (https://github.com/ceefluz/radar/blob/master/LICENSE)

# LIST OF REQUIRED PACKAGES -----------------------------------------------

required_packages <- c(
  "AMR",
  "broom",
  "DT",
  "ggrepel",
  "ggridges",
  "lubridate",
  "plotly",
  "RColorBrewer",
  "rintrojs",
  "shiny",
  "shinyBS",
  "shinycssloaders",
  "shinydashboard",
  "shinyjqui",
  "shinyjs",
  "shinyWidgets",
  "sjPlot", 
  "survival",
  "survminer",
  "tidyverse",
  "viridis"
) 

# install missing packages

new.packages <- required_packages[!(required_packages %in% installed.packages()[,"Package"])]

if (length(new.packages)) {
  install.packages(new.packages)
} 

# load all packages
lapply(required_packages, require, character.only = TRUE)


# LOAD DATASET ------------------------------------------------------------

set <- read_csv("") # Fill in path of dataset for analysis!

ab <- set %>%
  filter(!is.na(ab_type)) %>% 
  group_by(ab_type) %>%
  summarise(n = n()) %>%
  arrange(desc(n)) %>%
  filter(!is.na(ab_type)) %>%
  distinct()

ab_groups <- set %>% 
  filter(!is.na(ab_group)) %>% 
  select(ab_group) %>% 
  arrange(ab_group) %>% 
  distinct()


update_ab <- set %>% 
  select(ab_type, ab_group) %>% 
  distinct(.keep_all = TRUE)

set$year <- as.factor(set$year)




# HELP & INTRO DATA ---------------------------------------------------------------

steps <- read_csv2("help.csv")
intro <- read_csv2("intro.csv")

# SET COLORS --------------------------------------------------------------

radar_colors <- c(
  red = "#d33724",
  green = "#00b159",
  blue = "#00aedb",
  orange = "#f39c12",
  yellow = "#ffc425",
  lightgrey = "#cccccc",
  darkgrey = "#8c8c8c",
  bluegrey = "#42465e",
  male = "#d1351b", 
  female = "#f39c12"
)


radar_cols <- function(...) {
  
  cols <- c(...)
  
  if (is.null(cols)) {
    radar_colors
  } else {
    radar_colors[cols]  
  }
  
}

radar_palettes <- list(
  main  = radar_cols("blue", "green", "yellow"),
  cool  = radar_cols("blue", "green"),
  hot   = radar_cols("yellow", "red"),
  blue  = radar_cols("blue", "yellow"),
  mixed = radar_cols("blue", "green", "yellow", "orange", "red"),
  grey  = radar_cols("lightgrey", "bluegrey"),
  gender = radar_cols("gender")
)

radar_pal <- function(palette = "mixed", reverse = FALSE, ...) {
  pal <- radar_palettes[[palette]]
  
  if (reverse) {
    pal <- rev(pal)
  }
  
  colorRampPalette(pal, ...) 
}

scale_fill_radar <- function(palette = "mixed", discrete = TRUE, reverse = FALSE, ...) {
  pal <- radar_pal(palette = palette, reverse = reverse)
  
  if (discrete == TRUE) {
    discrete_scale("fill", paste0("radar_", palette), palette = pal, ...)
  } else {
    scale_fill_gradientn(colours = pal(256), ...)
  }
}


# FLUID DESIGN FUNCTION ---------------------------------------------------

fluid_design <- function(id, w, x, y, z) {
  fluidRow(
    div(
      id = id,
      column(
        width = 6,
        uiOutput(w),
        uiOutput(y)
      ),
      column(
        width = 6,
        uiOutput(x),
        uiOutput(z)
      )
    )
  )
}
