library(forsys)
library(tidyverse)
library(sf)

data("test_forest")
head(test_forest)

# example running forsys from json config file
jsonlite::fromJSON('misc/test_static_config.json')
forsys::run(config_file = 'misc/test_static_config.json', stands)

# example for calculating combined objective value
stands <- test_forest %>% st_drop_geometry() %>% mutate(priority12 = priority1 * priority2)

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
  global_threshold = "ownership == 2",
  scenario_output_fields = c("area_ha", "priority1", "priority2", "priority3", "priority4"),
  scenario_output_grouping_fields = "ownership",
  proj_fixed_target =  FALSE,
  proj_target_field = "area_ha",
  proj_target_value = 0.2
)

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

# plot project selects
proj_output <- outputs$project_output %>% dplyr::filter(Pr_1_priority1 == 1 & Pr_2_priority2 == 0)
plot_proj_dat <- test_forest %>%
  group_by(proj_id) %>% summarize() %>%
  dplyr::left_join(proj_output %>% dplyr::select(proj_id, treatment_rank))
plot(plot_proj_dat[,'treatment_rank'])

# plot stands selected
plot_proj <- test_forest %>% group_by(proj_id) %>% summarize() %>% st_geometry()
plot_stand_dat <- test_forest %>%
  select(stand_id, proj_id) %>%
  inner_join(outputs$stand_output %>% select(stand_id, ETrt_YR)) %>%
  left_join(outputs$project_output %>% select(proj_id, treatment_rank))
plot(plot_proj)
plot(plot_stand_dat[,'treatment_rank'], border=NA, key.pos = NULL, add=T)

plot(plot_proj)
plot(plot_proj_dat[,'treatment_rank'], key.pos = NULL, border=NA, add=T)
plot(plot_stand_dat[,'ETrt_YR'], border=NA, add=T, pal = colfunc(2))

colfunc <- colorRampPalette(c('black', NA))


# example run using using patchmax
adj = Patchmax::calculate_adj(Shapefile = test_forest, St_id = stands$stand_id, method = 'nb')
forsys::run(config_file = 'misc/test_patchmax_config.json', stand_data = stands, patchmax_stnd_adj = adj)


outputs = forsys::run(
  return_outputs = TRUE,
  stand_data = stands,
  scenario_name = "patchmax_test",
  stand_id_field = "stand_id",
  proj_id_field = "proj_id",
  stand_area_field = "area_ha",
  scenario_priorities = "priority1",
  stand_threshold = "priority3 >= 0.5",
  scenario_output_fields = c("area_ha", "priority1", "priority2", "priority3", "priority4"),
  scenario_output_grouping_fields = "ownership",
  run_with_patchmax = TRUE,
  patchmax_stnd_adj = adj,
  patchmax_proj_size = 25000,
  patchmax_proj_number = 5,
  patchmax_sample_n = 1000
)
