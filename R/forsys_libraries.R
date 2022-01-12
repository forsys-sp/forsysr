('Loading required R packages...')
pacman::p_load(
  class, # ? Patchmax ?
  data.table,
  directlabels,   # geom_dl
  dplyr,
  doParallel,     # Patchmax
  extRemes,
  foreign,        # DBF
  gtools,         # permutation
  jsonlite,       # Config files
  stringr,
  rgdal,
  rgeos,          # ?
  roxygen2,       # documentation
  scales, 
  sp,             # ? do we use
  sf,
  tidyverse
  )

