pacman::p_load(sf, dplyr)

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
  scenario_output_fields = c("Acres", "res_depart_PCP", "res_depart", "Am4CstBio"),
  run_with_patchmax = TRUE,
  patchmax_proj_size = 500,
  patchmax_proj_number = 10,
  patchmax_SDW = 1,
  patchmax_EPW = 0, 
  patchmax_sample_frac = 0.25,
  proj_target_field = "Am4CstBio",
  proj_target_value = 5000
)

proj <- run_outputs_2cons$project_output