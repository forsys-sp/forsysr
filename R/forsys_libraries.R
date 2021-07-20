('Loading required R packages...')
pacman::p_load(
  class, # ? Patchmax ? 
  data.table,
  directlabels,   # geom_dl
  dplyr,
  doParallel,     # Patchmax
  foreign,        # DBF
  gtools,         # permutations
  jsonlite,       # Config files
  leaflet,        # Drawing maps
  # maptools,     # ? superceeded by SF?
  shiny,
  shinyBS,        # Shiny tooltips
  shinyjs, 
  stringr, 
  rgdal,
  rgeos,          # ? 
  roxygen2,       # documentation
  sp,             # ? do we use
  sf,
  tidyverse
  )

