('Loading required R packages...')
#install.packages('pacman')
pacman::p_load(
  class, # ? Patchmax ? 
  data.table,
  directlabels, # geom_dl
  dplyr,
  doParallel, # Patchmax
  foreign, # DBF
  glue, # slightly different than paste0
  gtools, # permutations
  jsonlite, # Config files
  leaflet, # Drawing maps
  maptools, # ? superceeded by SF?
  shiny,
  shinyBS, # Shiny tooltips
  shinyjs, 
  stringr, # tidyverse
  rgdal,
  rgeos, # ? 
  roxygen2,
  sp, # ? do we use
  sf,
  tidyverse
  )

