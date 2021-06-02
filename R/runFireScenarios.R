run('config_TenYearPlan_WW_FS.R', dynamic_forsys = TRUE, fire_scenario = 1)
run('config_TenYearPlan_WW_FS.R', dynamic_forsys = TRUE, fire_scenario = 2)
run('config_TenYearPlan_WW_FS.R', dynamic_forsys = TRUE, fire_scenario = 3)

1:10 %>% purrr::map(~run('config_TenYearPlan_WW_FS.R', dynamic_forsys = TRUE, fire_scenario = .))

stnd_all <- list.files('output/WW_10yr_FS_80p/', pattern = 'stnd', full.names = T) %>% purrr::map_dfr(~read.csv(.))
write.csv(stnd_all, 'output/stnd_WW_10yr_FS_80p_FScn1_10.csv', row.names = F)

proj_all <- list.files('output/WW_10yr_FS_80p/', pattern = 'proj', full.names = T) %>% purrr::map_dfr(~read.csv(.))
write.csv(proj_all, 'output/proj_WW_10yr_FS_80p_FScn1_10.csv', row.names = F)
