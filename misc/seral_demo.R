pacman::p_load(sf, dplyr, forsys, patchmax)

shp <- st_read("~/Downloads/SERAL_sample/SERAL_sample.shp")


# Caluculate SPM and PCP for our priorities of interest
shp <- shp %>%
  forsys::calculate_spm(fields = c("res_depart")) %>%
  forsys::calculate_pcp(fields = c("res_depart"))

# Let's run forsys with a priority for departure but with a biomass constraint
# This doesn't make sense but who cares
set.seed(1234)
run_outputs_2cons = forsys::run(
  return_outputs = TRUE,
  write_outputs = TRUE,
  stand_data = shp,
  scenario_name = "patchmax_two_constraints",
  stand_id_field = "LMU_ID",
  stand_area_field = "Acres",
  scenario_priorities = c("res_depart_SPM"),
  global_threshold = 'OwnerClass == "USDA FOREST SERVICE"',
  scenario_output_fields = c("Acres", "res_depart_PCP", "res_depart", "Am4CstBio"),
  run_with_patchmax = TRUE,
  patchmax_proj_size = 500,
  patchmax_proj_number = 10,
  patchmax_SDW = 1,
  patchmax_EPW = 0, 
  patchmax_sample_frac = 0.25,
  proj_target_field = "Am4CstBio",
  proj_target_value = 5000, 
  patchmax_exclusion_limit = 1
)

proj <- run_outputs_2cons$project_output




shp <- shp %>%
  calculate_pcp(fields = c('Am4RevBio','prob_8p','res_depart')) %>%
  combine_priorities(
    fields = c('Am4RevBio_PCP','prob_8p_PCP','res_depart_PCP'), 
    weights = c(1,1,1), 
    new_field = 'preset_priority')

run_outputs_preset = forsys::run(
  return_outputs = TRUE,
  write_outputs = TRUE,
  stand_data = shp,
  scenario_name = "patchmax_forsys_preset",
  stand_id_field = "LMU_ID",
  stand_area_field = "Acres",
  # proj_id_field = "POD",
  proj_fixed_target = TRUE,
  proj_target_field = "Acres",
  proj_target_value = 500,
  annual_target_field = "Acres",
  annual_target_value = 2000,
  global_threshold = 'OwnerClass == "USDA FOREST SERVICE"',
  stand_threshold = 'Am4RevBio > 500',
  scenario_priorities = c("preset_priority"),
  scenario_output_fields = c("Acres", "Am4RevBio_PCP", "prob_8p_PCP", "res_depart_PCP"),
  run_with_patchmax = TRUE,
  patchmax_proj_size = 500,
  patchmax_proj_number = 10,
  patchmax_SDW = 1,
  patchmax_EPW = 0, 
  patchmax_exclusion_limit = 0.4,
  patchmax_sample_frac = 0.25
)

run_outputs_preset$project_output
