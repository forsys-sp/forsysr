library(shiny)

source('R/forsys_libraries.R')
source('R/forsys_functions.R')
source('R/forsys_scenario_functions.R')
source('R/forsys_results_functions.R')
source('R/ForSys.R')
source('ui.R')
source('server.R')

###########################################
## Create intersect table FORSYS W/ FIRE ##
###########################################

# see R/fire_calc_fsim_intersect.R
# creates stand_fire_intersect table used in ForSys w/ Fire

#########################################
## Parameters for FORSYS W/ FIRE       ##
#########################################

# these parameters can be included within the config file if desired

source('R/fire_misc_func.R')

#input_stand_fire_intersect <- 'data/hexnet_west_fsim19_30reps_intersect.csv'
fire_intersect_table <- '~/Dropbox/!!projects/aa_10yr_uncertainity/data/west_usfs_hexnet_fsim19_omernik6x_deciles5x_futures20x_intersect.csv'
if(exists('f_df') == FALSE) f_df <- fread(fire_intersect_table)

# !! planning_years, annual_project_target, annual_target_field defined in the config file

#########################################
## Run FORSYS W/ FIRE                  ##
#########################################

# run single scenario ...

run('config_TenYearFirePlan_WW_FS.R',
    fire_dynamic_forsys = TRUE,
    write_tags = data.frame('DECILE' = 5, 'FUTURE' = 1),
    fire_intersect_table = f_df %>% filter(DECILE == 5, FUTURE == 1))

# run multiple scenarios using purrr ...

fire_scn_inputs = expand.grid(SCENARIO = c('Dyn','Std','Ran'),
                              DECILE = 5:9,
                              FUTURE = 1:2,
                              stringsAsFactors = F)
fire_scn_inputs %>% pmap(function(...){
  scn_i = tibble(...)
  fires_i <- fread(input_stand_fire_intersect) %>% right_join(scn_i) %>% dplyr::select(-n, -PA_ID, -OWNER, -AREA_HA)
  run(config_file = 'config_TenYearPlan_WW_FS.R',
      fire_dynamic_forsys = (scn_i$SCENARIO == 'Dyn'),
      fire_random_projects = (scn_i$SCENARIO == 'Ran'),
      fire_intersect_table = fires_i, write_tags=scn_i)
})

#########################################
## Post-process FORSYS W/ FIRE outputs ##
#########################################

# combine stand level outputs & tag w/ forsys type
process_outputs('config_TenYearPlan_WW_FS.R',
                scenario_param_columns = c('SCENARIO','DECILE','FUTURE'),
                fires_dat = fread(input_stand_fire_intersect) %>%
                  dplyr::select(-n, -PA_ID, -OWNER, -AREA_HA))

#########################################
## Animate FORSYS W/ FIRE outputs     ##
#########################################

# see R/fire_misc_animations.R
# tmp <- fread('output/WW_10yr_FS_80p/_stnd_combined.csv')
# tmp %>% group_by(DECILE, FUTURE) %>% summarize(AREA_HA = sum(AREA_HA, na.rm=T))



