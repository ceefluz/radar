
library(tidyverse)
library(lubridate)

set <- read_csv("set_clean_sample.csv")

ab <- set %>%
  group_by(ab_type) %>%
  summarise(n = n()) %>%
  arrange(-n) %>%
  filter(!is.na(ab_type)) %>%
  distinct()

ab_group <- set %>% 
  select(ab_group) %>% 
  arrange(ab_group) %>% 
  distinct()


update_ab <- set %>% 
  select(ab_type, ab_group) %>% 
  distinct(.keep_all = TRUE)

set$year <- factor(set$year)

# HELP & INTRO DATA ---------------------------------------------------------------

steps <- read_delim("help.csv", ";", escape_double = FALSE, trim_ws = TRUE)
intro <- read_delim("intro.csv", ";", escape_double = FALSE, trim_ws = TRUE)

# SET COLORS --------------------------------------------------------------

radar_colors <- c(
  red        = "#d33724",
  green      = "#00b159",
  blue       = "#00aedb",
  orange     = "#f39c12",
  yellow     = "#ffc425",
  lightgrey  = "#cccccc",
  darkgrey   = "#8c8c8c")

radar_cols <- function(...) {
  cols <- c(...)
  
  if (is.null(cols))
    return (radar_colors)
  
  radar_colors[cols]
}

radar_palettes <- list(
  main  = radar_cols("blue", "green", "yellow"),
  
  cool  = radar_cols("blue", "green"),
  
  hot   = radar_cols("yellow", "red"),
  
  blue = radar_cols("blue", "yellow"),
  
  mixed = radar_cols("blue", "green", "yellow", "orange", "red"),
  
  grey  = radar_cols("#edeff4", "#42465e")
)

radar_pal <- function(palette = "mixed", reverse = FALSE, ...) {
  pal <- radar_palettes[[palette]]
  
  if (reverse) pal <- rev(pal)
  
  colorRampPalette(pal, ...)
}

scale_fill_radar <- function(palette = "mixed", discrete = TRUE, reverse = FALSE, ...) {
  pal <- radar_pal(palette = palette, reverse = reverse)
  
  if (discrete) {
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
