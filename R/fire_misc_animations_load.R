
# STEP 2: compile forsys results ...

# reference data...
if(exists('f_df') == FALSE) f_df <- data.table::fread('data/hexnet_west_fsim19_30reps_intersect.csv')
if(exists('hex') == FALSE) hex <- fread('data/hexnet_west_fs.csv')

# Misc animation workshop

hex_p <- hex %>%
  st_as_sf(coords = c("POINT_X", "POINT_Y"), crs = 5070)

p_df <- hex %>% filter(OwnerCat == 'USFS') %>% group_by(PA_ID) %>% summarize(AREA_HA = sum(AREA_HA))

states_w11 <- ne_states(country = 'united states of america', returnclass = 'sf') %>%
  filter(region == 'West') %>%
  filter(postal %in% c('HI','AK') == FALSE) %>%
  dplyr::select(postal) %>%
  st_transform(5070)

states <- ne_states(country = 'united states of america', returnclass = 'sf') %>%
  filter(postal %in% c('HI','AK') == FALSE) %>%
  dplyr::select(postal) %>%
  st_transform(5070)

usfs <- st_read('../../Dropbox/!gis/boundaries/usfs_boundaries_western.shp') %>%
  st_geometry() %>% st_transform(5070)

prj <- st_read('../../Dropbox/!gis/Firesheds_CONUS.gdb/', 'ProjectAreas') %>%
  st_geometry() %>% st_transform(5070)

ny <- st_read('../../Dropbox/!gis/north_yuba_watershed.geojson')
