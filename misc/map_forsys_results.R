proj_geom <- st_read('~/Dropbox/!gis/Firesheds_CONUS.gdb/', 'ProjectAreas') %>% rmapshaper::ms_simplify(keep = 0.2) %>% st_transform(4326)
usfs_geom <- st_read('~/Dropbox/!gis/shp_boundaries/usfs_boundaries_western.shp') %>% st_buffer(-500) %>% st_transform(4326)

map_projects <- function(prioritized_proj, proj_geom, field){
  map_dat <- proj_geom %>%
    dplyr::inner_join(prioritized_proj, by='PA_ID')
  col_func <- colorNumeric(palette = 'YlOrRd', domain = NULL)
  if(field == 'treatment_rank')
    col_func <- colorNumeric(palette = 'YlOrRd', domain = NULL, reverse = T)
  leaflet() %>%
    addMapPane('base_pane', zIndex = 440) %>%
    addMapPane('proj_pane', zIndex = 450) %>%
    addMapPane('label_pane', zIndex = 450) %>%
    addProviderTiles('CartoDB.PositronNoLabels', providerTileOptions(minZoom = 4, maxZoom = 15), options = pathOptions(pane = "base_pane"), group='base') %>%
    addProviderTiles('Esri.WorldImagery', providerTileOptions(minZoom = 4, maxZoom = 15), options = pathOptions(pane = "base_pane"), group='imagery') %>%
    addPolygons(data=usfs_geom, color='black', fillColor='grey', opacity=0.1, weight=1, fillOpacity = 0.1, options = pathOptions(pane = "base_pane")) %>%
    addPolygons(data=map_dat, color='black', fillColor=~col_func(get(field)), opacity=0.1, weight=1, fillOpacity = 0.7, label=~get(field), options = pathOptions(pane = "proj_pane"), group='projects') %>%
    addProviderTiles('CartoDB.PositronOnlyLabels', options = pathOptions(pane = "label_pane")) %>%
    addLegend(layerId = 'legend_own', data = map_dat, pal = col_func, values = ~get(field), title = field) %>%
    addLayersControl(baseGroups = c('base',"imagery"), overlayGroups = c('projects'), options = layersControlOptions(collapsed = F))
}

prioritized_proj <- read.csv('output/WW_10yr_FS_80p/proj_WW_10yr_FS_80p_.csv')
map_projects(prioritized_proj, proj_geom, 'ETrt_AREA_HA')
map_projects(prioritized_proj, proj_geom, 'ETrt_FireDef')
map_projects(prioritized_proj, proj_geom, 'treatment_rank')


stand_output <- read.csv('output/WW_10yr_FS_80p/DECILE_5_FUTURE_1/stnd_WW_10yr_FS_80p_DECILE_5_FUTURE_1.csv')
xtabs(stand_output$AREA_HA ~ stand_output$FIRE_YR)
xtabs(stand_output$AREA_HA ~ stand_output$ETrt_YR)/1e6
