if (!require(remotes)) install.packages("remotes")
remotes::install_github("forsys-sp/forsysr", auth_token = 'your_token_here')
remotes::install_github("forsys-sp/patchmax", auth_token = 'your_token_here')

# STATIC PROJECTS ------------------------

library(tidyverse)
library(forsys)
library(sf)

data("test_forest")
head(test_forest)

# plot the treatment units
plot(test_forest[,c(4:5,7:10)], border=NA)

# example for exploring two priorities
stands <- test_forest %>% st_drop_geometry()

# example running forsys from json config file
jsonlite::fromJSON('misc/test_static_config.json')
forsys::run(config_file = 'misc/test_static_config.json', stand_data = stands)

# run forsys using specified parameters (see help for complete list)
# prioritize priority1 AND priority2 within predefined boundaries (proj_id)
# group outputs by ownership
# treat 20% of the area within each predefined boundary (proj_id)
outputs = forsys::run(
  return_outputs = TRUE,
  stand_data = stands,
  scenario_name = "static_test",
  stand_id_field = "stand_id",
  proj_id_field = "proj_id",
  stand_area_field = "area_ha",
  scenario_priorities = c("priority1", "priority2"),
  scenario_weighting_values = "0 5 1",
  stand_threshold = "threshold1 == 1",
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

# plot project where only priority1 is prioritized
# to see priority2 flip argument: (Pr_1_priority1 == 0 & Pr_2_priority2 == 1)
# t0 see where the two priorities are equally weighted replace with this argument:
#  (Pr_1_priority1 == 1 & Pr_2_priority2 == 1)
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
  inner_join(outputs$stand_output %>% select(stand_id)) %>%
  left_join(outputs$project_output %>% select(proj_id, treatment_rank))

ggplot() + 
  geom_sf(data = plot_proj_dat, aes(fill=treatment_rank)) +
  geom_sf(data = plot_stand_dat %>% st_centroid())

colfunc <- colorRampPalette(c('black', NA))

# USING PATCHMAX ------------------------

data("test_forest")
stands <- test_forest %>% st_drop_geometry() 

# first we need to create an object describing stand adjacency and for gridded data, distance
# the distance function takes a few minutes
adj = Patchmax::calculate_adj(test_forest, St_id = test_forest$stand_id, method='nb')
dist = Patchmax::calculate_dist(test_forest)

# run patchmax by specifying parameters
outputs = forsys::run(
  return_outputs = TRUE,
  stand_data = stands,
  scenario_name = "patchmax_test",
  stand_id_field = "stand_id",
  proj_id_field = "proj_id",
  stand_area_field = "area_ha",
  scenario_priorities = "priority2",
  stand_threshold = "priority3 >= 0.5",
  scenario_output_fields = c("area_ha", "priority1", "priority2", "priority3", "priority4"),
  run_with_patchmax = TRUE,
  patchmax_stnd_adj = adj,
  patchmax_proj_size = 25000,
  patchmax_proj_number = 10,
  patchmax_st_distance = dist,
  patchmax_SDW = 10
)

plot_patch <- outputs$stand_output %>% mutate(treatment_rank = proj_id)
plot_patch <- test_forest %>% left_join(plot_patch %>% select(stand_id, treatment_rank)) %>%
  group_by(treatment_rank) %>% summarize()
plot(plot_patch[,'treatment_rank'], border=NA, main="Patch rank")
