run('config_TenYearPlan_WW_FS.R', dynamic_forsys = TRUE, fire_scenario = 1)
run('config_TenYearPlan_WW_FS.R', dynamic_forsys = TRUE, fire_scenario = 2)
run('config_TenYearPlan_WW_FS.R', dynamic_forsys = TRUE, fire_scenario = 3)

run('config_TenYearPlan_WW_FS.R', dynamic_forsys = TRUE, fire_scenario = 1)
run('config_TenYearPlan_WW_FS.R', dynamic_forsys = FALSE, fire_scenario = 1)
run('config_TenYearPlan_WW_FS.R', dynamic_forsys = FALSE, random_projects = TRUE, fire_scenario = 1)


1:10 %>% purrr::map(~run('config_TenYearPlan_WW_FS.R', dynamic_forsys = TRUE, fire_scenario = .))
1:10 %>% purrr::map(~run('config_TenYearPlan_WW_FS.R', dynamic_forsys = FALSE, fire_scenario = .))
1:10 %>% purrr::map(~run('config_TenYearPlan_WW_FS.R', random_projects = TRUE, fire_scenario = .))


f_df <- read_delim(input_stand_fire_intersect, delim=',')
load('../../Dropbox/!!projects/!archive/aa_10yr/hex.Rdata')
# hex_p <- hex %>%
#   filter(OwnerCat == 'USFS') %>%
#   st_as_sf(coords = c("POINT_X", "POINT_Y"), crs = 5070)

stnd_all <- list.files('output/WW_10yr_FS_80p/', pattern = 'stnd.*Ran', full.names = T) %>%
  purrr::map_dfr(~read.csv(.)) %>%
  full_join(f_df %>% filter(OWNER == 'USFS') %>% dplyr::select(-AREA_HA) %>% filter(SCN_ID %in% c(1:10))) %>%
  mutate(EVENT = case_when(
    ETrt_YR <= FIRE_YR ~ 'T_F',
    FIRE_YR < ETrt_YR ~ 'F_T',
    is.na(ETrt_YR) & !is.na(FIRE_YR) ~ 'F',
    is.na(FIRE_YR) & !is.na(ETrt_YR) ~ 'T',
    is.na(ETrt_YR) & is.na(FIRE_YR) ~ 'N'
  ))
  suppressWarnings(
    stnd_all$YEAR <- stnd_all %>% dplyr::select(ETrt_YR, FIRE_YR) %>% apply(1, min, na.rm=T)
  )

stnd_all_out <- stnd_all %>%
  arrange(SCN_ID, YEAR, PA_ID, CELL_ID) %>%
  select(-AREA_HA, -aTR_MS, -OWNER) %>%
  left_join(hex %>% dplyr::select(CELL_ID, AREA_HA, aTR_MS)) %>%
  mutate(AREA_HA = round(AREA_HA), aTR_MS = round(aTR_MS, 4)) %>%
  relocate(SCN_ID, YEAR, PA_ID)

stnd_all_out$SCN_ID %>% table()
stnd_all_out$EVENT %>% table()

write.csv(stnd_all_out, 'output/stnd_WW_10yr_FS_80p_Rnd_FScn1_10.csv', row.names = F)

proj_all <- list.files('output/WW_10yr_FS_80p/', pattern = 'proj.*Dyn', full.names = T) %>% purrr::map_dfr(~read.csv(.))
write.csv(proj_all, 'output/proj_WW_10yr_FS_80p_Dyn_FScn1_10.csv', row.names = F)

proj_all <- list.files('output/WW_10yr_FS_80p/', pattern = 'proj.*Std', full.names = T) %>% purrr::map_dfr(~read.csv(.))
write.csv(proj_all, 'output/proj_WW_10yr_FS_80p_Std_FScn1_10.csv', row.names = F)

proj_all <- list.files('output/WW_10yr_FS_80p/', pattern = 'proj.*Ran', full.names = T) %>% purrr::map_dfr(~read.csv(.))
write.csv(proj_all, 'output/proj_WW_10yr_FS_80p_Rnd_FScn1_10.csv', row.names = F)
