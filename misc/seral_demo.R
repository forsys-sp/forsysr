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
  patchmax_sample_frac = 1,
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




# EXAMPLE SELECTING PATCHS BY POD

plot(shp[,'POD'])
table(shp$POD)
PODs = c(215, 216, 219, 220)
i = PODs[1]

combined_outputs <- PODs %>% purrr::map(function(i){
  
  print(i)
  shp_i = shp %>% filter(POD == i)
  sum(shp_i$Acres)
  run_outputs = forsys::run(
    return_outputs = TRUE,
    write_outputs = TRUE,
    stand_data = shp_i,
    scenario_name = "patch_in_run_test",
    stand_id_field = "LMU_ID",
    stand_area_field = "Acres",
    scenario_priorities = c("res_depart_SPM"),
    scenario_output_fields = c("Acres", "Am4RevBio_PCP", "prob_8p_PCP", "res_depart_PCP", "res_depart","Am4CstBio"),
    run_with_patchmax = TRUE,
    patchmax_proj_size = 1000,
    patchmax_proj_size_min = 300,
    patchmax_proj_number = 10,
    # patchmax_proj_size_min = 9000,
    patchmax_SDW = 1,
    patchmax_sample_frac = 1
  )
  
  run_outputs[[1]]$POD <- i
  return(run_outputs)
})

length(combined_outputs)
tmp <- combined_outputs |> 
  purrr::map_dfr(1) |>
  # mutate(proj_id = paste0(POD, '_', proj_id)) |>
  mutate(LMU_ID = as.numeric(LMU_ID))

tmp |> 
  group_by(POD, proj_id) |>
  summarize(Acres = sum(Acres))

tmp |> 
  group_by(POD) |>
  summarize(patch_cnt = n_distinct(proj_id))

shp2 <- shp |> left_join(tmp |> select(LMU_ID, proj_id))
shp_POD <- shp |> group_by(POD) |> summarize()
plot(shp2[,'proj_id'], key.pos = 4, key.length = .5)
plot(shp_POD, add=T, lwd=3)

library(ggplot2)

ggplot() +
  geom_sf(data = shp2, aes(fill = proj_id)) +
  scale_fill_viridis_c() +
  geom_sf(data = shp_POD, fill=NA, lwd = 2, color = 'black')