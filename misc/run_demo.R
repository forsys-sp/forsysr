# if (!require(remotes)) install.packages("remotes")
# remotes::install_github("forsys-sp/forsysr", auth_token = 'your_token_here')
# remotes::install_github("forsys-sp/patchmax", auth_token = 'your_token_here')

# STATIC PROJECTS ------------------------

library(tidyverse)
library(forsys)
library(sf)
library(dplyr)
library(future)


data(test_forest)
data(test_fire)
test_forest_no_geom <- st_drop_geometry(test_forest)

year_df <- data.frame(year = unique(fire_intersect$year))
year_df$new_year <- sample(1:100, nrow(year_df), T)
fire_intersect$year <- year_df$new_year[match(fire_intersect$year, year_df$year)]


# FORSYS test A: single-priority static
test_a_config <- 'configs/test_a_static_config.json'
print_json_config(test_a_config)
# edit_json_config(test_a_config)
test_a <- forsys::run(config_file = test_a_config, 
                 stand_data = test_forest_no_geom, 
                 return_outputs = T)

# FORSYS test B: multi-priority static
test_b_config <- 'configs/test_b_static_multi_priority_config.json'
print_json_config(test_a_config)
test_b <- forsys::run(config_file = test_b_config, 
                 stand_data = test_forest_no_geom, 
                 return_outputs = T)

# FORSYS test C: single-priority static with fire
test_c_config <- 'configs/test_c_static_w_fire_config.json'
print_json_config(test_c_config)
test_c <- forsys::run(config_file = test_c_config, 
                      stand_data = test_forest_no_geom,
                      fire_intersect_table = fire_intersect,
                      return_outputs = T)

# FORSYS test D: single-priority dynamic with fire
test_d_config <- 'configs/test_d_patchmax_config.json'
print_json_config(test_d_config)
future::plan(future::multisession, workers=8)
test_d <- forsys::run(config_file = test_d_config, 
            stand_data = test_forest,
            fire_intersect_table = fire_intersect, 
            run_with_fire = T, 
            planning_years = 3, 
            fire_year_field = 'year', 
            return_outputs = T)

# FORSYS test E: multi-priority dynamic
test_e_config <- 'configs/test_e_patchmax_multi_priority_config.json'
print_json_config(test_e_config)
future::plan(future::multisession, workers=8)
test_e <- forsys::run(config_file = test_e_config, 
                      stand_data = test_forest,
                      return_outputs = T)

# FORSYS test F: single-priority dynamic with fire
test_f_config <- 'configs/test_f_patchmax_w_fire_config.json'
print_json_config(test_f_config)
future::plan(future::multisession, workers=8)
test_f <- forsys::run(config_file = test_f_config, 
                      stand_data = test_forest,
                      fire_intersect_table = fire_intersect, 
                      return_outputs = T)


combine_priorities(
  stands = test_forest, 
  fields = c('priority1','priority2','priority3'), 
  weights = c(3,1,1), 
  new_field = 'new_priority')

filter_stands(test_forest, filter_txt = 'mosaic1 == 3')

# calculate and append SPM and PCP values
test_forest <- forsys::test_forest %>% 
  calculate_spm(fields = c("priority1","priority2"), availability_txt = 'mosaic1 == 3') %>%
  calculate_pcp(fields = c("priority1","priority2"), availability_txt = 'mosaic1 == 3') %>%
  combine_priorities(fields = c("priority1_SPM","priority2_SPM"))


# run forsys using specified parameters (see help for complete list)
# prioritize priority1 AND priority2 within predefined boundaries (proj_id)
# group outputs by ownership
# treat 20% of the area within each predefined boundary (proj_id)
outputs = forsys::run(
  return_outputs = TRUE,
  write_outputs = TRUE,
  stand_data = st_drop_geometry(test_forest),
  scenario_name = "run_static_test_2",
  stand_id_field = "stand_id",
  proj_id_field = "proj_id",
  stand_area_field = "area_ha",
  scenario_priorities = c("priority1", "priority2"),
  scenario_weighting_values = "0 5 1",
  stand_threshold = "threshold1 == 1",
  scenario_output_fields = c("area_ha", "priority1", "priority2", "priority3", "priority4"),
  scenario_output_grouping_fields = "mosaic2",
  proj_fixed_target =  FALSE,
  proj_target_field = "area_ha",
  proj_target_value = 0.2
)

outputs$project_output %>% filter(Pr_1_priority1 == 3, Pr_2_priority2 == 2)

# GRAPH OUTPUT ------------------------

outputs <- test_a

theme_toggle('light')
# built in graphing examples

# plot attainment curves
forsys::attainment_chart_by_target_treated(
  results_data = outputs$project_output,
  priority = 'priority3',
  constraint_field = 'area_ha',
  secondary = 'priority1')

# stacked attainment curved
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
  group_field = 'mosaic2'
)

# MAP OUTPUT ------------------------

# plot project where only priority1 is prioritized
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
  mutate(stand_id = as.character(stand_id)) %>%
  inner_join(outputs$stand_output %>% select(stand_id)) %>%
  left_join(outputs$project_output %>% select(proj_id, treatment_rank))

ggplot() + 
  geom_sf(data = plot_proj_dat, aes(fill=treatment_rank)) +
  geom_sf(data = plot_stand_dat %>% st_centroid())

colfunc <- colorRampPalette(c('black', NA))

# USING PATCHMAX ------------------------
library(patchmax)
library(tidyverse)
library(future)

data("test_forest")
stands <- forsys::test_forest

# unlike the example above, patchmax requires the stand data to contain geometry
plot(stands$geometry)

plan(multisession, workers=8)

# run patchmax by specifying parameters
outputs = forsys::run(
  return_outputs = TRUE,
  write_outputs = TRUE,
  stand_data = stands,
  scenario_name = "patchmax_test",
  stand_id_field = "stand_id",
  proj_id_field = "proj_id",
  stand_area_field = "area_ha",
  scenario_priorities = "priority1",
  # scenario_priorities = c("priority1","priority2"),
  stand_threshold = "priority4 >= 0.3",
  scenario_output_fields = c("area_ha", "priority1", "priority2", "priority3", "priority4"),
  # scenario_weighting_values = "0 1 1",
  run_with_patchmax = TRUE,
  proj_target_field = 'priority4',
  proj_target_value = Inf,
  patchmax_proj_size = 25000,
  patchmax_proj_number = 2,
  patchmax_SDW = .5,
  patchmax_EPW = .5,
  patchmax_sample_frac = 0.1,
)

patch_sf <- stands %>%
  mutate(stand_id = as.character(stand_id)) %>%
  left_join(outputs$stand_output %>% dplyr::select(stand_id, treatment_rank = proj_id)) %>%
  group_by(treatment_rank) %>% summarize_if(is.numeric, sum) %>% st_as_sf() %>%
  filter(treatment_rank %in% c(1:2))

availability_sf <- stands %>% filter(priority3 > 0.5) %>% summarize()

ggplot() + 
  geom_sf(data=stands, aes(fill=priority1), color=NA, alpha=0.5) +
  geom_sf(data=patch_sf, aes(color=factor(treatment_rank)), fill=NA, linewidth = 3) +
  geom_sf(data=availability_sf, fill=NA, color='white', lty=3) +
  scale_fill_gradientn(colors=sf.colors(10))
