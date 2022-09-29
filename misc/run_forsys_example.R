# STATIC PROJECTS ------------------------

library(tidyverse)
library(forsys)
library(sf)

data("test_forest")
head(test_forest)

# example for calculating combined objective value
stands <- test_forest %>% st_drop_geometry() %>% mutate(priority12 = priority1 * priority2)

# example running forsys from json config file
# jsonlite::fromJSON('misc/test_static_config.json')
# forsys::run(config_file = 'misc/test_static_config.json', stand_data = stands)

# run forsys using specified parameters (see help for complete list)
outputs = forsys::run(
  return_outputs = TRUE,
  stand_data = stands,
  scenario_name = "static_test",
  stand_id_field = "stand_id",
  proj_id_field = "proj_id",
  stand_area_field = "area_ha",
  scenario_priorities = c("priority1", "priority2"),
  scenario_weighting_values = "0 3 1",
  stand_threshold = "threshold1 == 1",
  # global_threshold = "ownership == 2",
  scenario_output_fields = c("area_ha", "priority1", "priority2", "priority3", "priority4"),
  scenario_output_grouping_fields = "ownership",
  proj_fixed_target =  FALSE,
  proj_target_field = "area_ha",
  proj_target_value = 0.2
)

# GRAPH OUTPUT ------------------------

theme_toggle('light')
# built in graphing examples

# plot attainment curves
forsys::attainment_chart_by_target_treated(
  results_data = outputs$project_output,
  priority = 'priority1',
  constraint_field = 'area_ha',
  secondary = 'priority2')

# stacked attainment curvsd
forsys::cumulative_attainment_chart(
  results_data = outputs$project_output,
  priority = 'priority1',
  constraint_field = 'area_ha',
  secondary_effects = 'priority2')

# trade off analysis (requires multiple priorities)
forsys::tradeoff_analysis_chart(
  results_data = outputs$project_output,
  proj_field = 'proj_id',
  x_field = 'priority1',
  y_field = 'priority2')

# project rank analysis (requires multiple priorities)
forsys::project_boxplot(
  results_data = outputs$project_output,
  proj_field = 'proj_id',
  x_field = 'priority1',
  y_field = 'priority2',
  constraint_field = 'area_ha'
)

# project attainment by secondary category plot
forsys::stacked_barchart(
  subset_data = outputs$subset_output,
  proj_field = 'proj_id',
  priority = 'priority1',
  group_field = 'ownership'
)

# MAP OUTPUT ------------------------

# plot project
proj_output <- outputs$project_output %>% 
  dplyr::filter(Pr_1_priority1 == 1 & Pr_2_priority2 == 0)

plot_proj_dat <- test_forest %>%
  group_by(proj_id) %>% summarize() %>%
  dplyr::left_join(proj_output %>% dplyr::select(proj_id, treatment_rank))

ggplot() + 
  geom_sf(data = plot_proj_dat, aes(fill=treatment_rank)) 

# plot stands selected
plot_proj <- test_forest %>% 
  group_by(proj_id) %>% 
  summarize() %>% 
  st_geometry()

plot_stand_dat <- test_forest %>%
  select(stand_id, proj_id) %>%
  inner_join(outputs$stand_output %>% select(stand_id, ETrt_YR)) %>%
  left_join(outputs$project_output %>% select(proj_id, treatment_rank))

ggplot() + 
  geom_sf(data = plot_proj_dat, aes(fill=treatment_rank)) +
  geom_sf(data = plot_stand_dat %>% st_centroid())

colfunc <- colorRampPalette(c('black', NA))

# USING PATCHMAX ------------------------

data("test_forest")
stands <- test_forest %>% 
  st_drop_geometry() %>% 
  mutate(priority12 = priority1 * priority2)

adj = Patchmax::calculate_adj(Shapefile = test_forest, 
                              St_id = stands$stand_id, 
                              Adjdist = 100, 
                              method = 'buffer')

# run patchmax using config file
forsys::run(config_file = 'misc/test_patchmax_config.json', 
            stand_data = stands, 
            patchmax_stnd_adj = adj)

# fun patchmax by specifiying parameters
outputs = forsys::run(
  return_outputs = TRUE,
  stand_data = stands,
  scenario_name = "patchmax_test",
  stand_id_field = "stand_id",
  proj_id_field = "proj_id",
  stand_area_field = "area_ha",
  scenario_priorities = "priority12",
  stand_threshold = "priority3 >= 0.5",
  scenario_output_fields = c("area_ha", "priority1", "priority2", "priority3", "priority4"),
  scenario_output_grouping_fields = "ownership",
  run_with_patchmax = TRUE,
  patchmax_stnd_adj = adj,
  patchmax_proj_size = 25000,
  patchmax_proj_number = 5,
  patchmax_sample_n = 1000
)


# USING SERAL DATA ------------------------

library(forsys)
library(tidyverse)
library(sf)

# example for calculating combined objective value
stands <- st_read('~/GitHub/forsys-data/YSS_LandMgtUnits/YSS_LandMgtUnits_v16_20210924.shp') 
adj = Patchmax::calculate_adj(Shapefile = stands, St_id = stands$LMU_ID, method = 'buffer', Adjdist = 1)
stands_dat <- stands %>% st_drop_geometry()

# run forsys using specified parameters (see help for complete list)
outputs = forsys::run(
  return_outputs = TRUE,
  stand_data = stands,
  scenario_name = "YSS_test",
  stand_id_field = "LMU_ID",
  proj_id_field = "POD",
  stand_area_field = "Acres",
  scenario_priorities = c("TotConVol"),
  scenario_weighting_values = "0 3 1",
  scenario_output_fields = c("Acres", "TotConVol"),
  scenario_output_grouping_fields = "Forest_Typ",
  proj_fixed_target =  FALSE,
  proj_target_field = "Acres",
  proj_target_value = 0.2
)


outputs = forsys::run(
  return_outputs = TRUE,
  stand_data = stands,
  scenario_name = "yss_patchmax_test",
  stand_id_field = "LMU_ID",
  proj_id_field = "POD",
  stand_area_field = "Acres",
  scenario_priorities = "TotConVol",
  stand_threshold = "TotConVol > 0",
  global_threshold = "Forest_Typ == 'Mixed conifer/fir'",
  scenario_output_fields = c("Acres", "TotConVol"),
  run_with_patchmax = TRUE,
  patchmax_stnd_adj = adj,
  patchmax_proj_size = 25000,
  patchmax_proj_number = 5,
  patchmax_sample_n = 1000
)



# USING EL DORADO DATA ------------------------

library(forsys)
library(tidyverse)
library(sf)

# example for calculating combined objective value
eldor <- st_read('~/GitHub/forsys-data/El_Dorado_Poly/El_Dorado_Poly.shp') 

unique(eldor$PODid)

i = 10
adj = Patchmax::calculate_adj(Shapefile = eldor, St_id = eldor$CELL_ID, method = 'buffer', Adjdist = 1)
stands_dat <- stands %>% st_drop_geometry()


# for each POD i....
# combined_outputs <- unique(eldor$PODid)[1:5] %>% 
combined_outputs <- c(10,19,25,44,32) %>%
  purrr::map(function(i){
    
    # filter stands to i and calculate adjacency
    print(i)
    eldor_i = eldor %>% filter(PODid == i)
    stands_i <- eldor_i %>% st_drop_geometry()
    adj_i = eldor_i %>% Patchmax::calculate_adj(St_id = .$CELL_ID) # << now defaults to buffer approach
    
    # run forsys w/ patchmax for pod i
    outputs_i = forsys::run(
      return_outputs = TRUE,
      stand_data = stands_i,
      scenario_name = "eldor_pod40",
      stand_id_field = "CELL_ID",
      proj_id_field = "patchmax_id",
      stand_area_field = "Acres",
      scenario_priorities = c("prio_fire"),
      stand_threshold = "Available >= 1",
      #global_threshold = "Available == 1",
      scenario_output_fields = c("Acres", "Threat_Ac", "Defend_Ac", "HVRAres_Ac"),
      scenario_output_grouping_fields = c("PODid","priority"),
      run_with_patchmax = TRUE,
      patchmax_stnd_adj = adj_i,
      patchmax_proj_size = 100,
      patchmax_proj_number = 2,
      patchmax_sample_n = 100
    )
    return(outputs_i)
  })

# extract first element (i.e., stands) in combined_outputs list and bind rows
combined_outputs %>% 
  purrr::map(2) %>% 
  bind_rows()
