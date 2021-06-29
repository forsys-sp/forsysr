setwd('C:/Users/houtmanr/ForSysRepo/')
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
# outputs stand_fire_intersect table to data directory

#########################################
## Parameters for FORSYS W/ FIRE       ##
#########################################

# these parameters can be included within the config file if desired.

source('R/fire_misc_func.R')

input_stand_fire_intersect <- 'data/west_usfs_hexnet_fsim19_omernik6x_deciles5x_futures20x_intersect.csv'
#input_stand_fire_intersect <- 'data/hexnet_west_fsim19_30reps_intersect.csv'


input_stand <- 'data/hexnet_west_fs.csv'

if(exists('f_df') == FALSE) f_df <- fread(input_stand_fire_intersect)
if(exists('hex') == FALSE) hex <- fread(input_stand)

# number of simulation years
planning_years = 5

# 10-year ramp-up
#annual_project_target = logisticFunc(1:10, start= 0, end=1.228e6)

# 10-year ramp (6.6 M ha treated)
max_rx_rate = 1200000 # maximum 1.2 million ha per year
annual_project_target = max_rx_rate * logisticFunc(yr = 1:10, mid = 5, normalize = T)

# 20-year plan w/ 10-year ramp-up
#annual_project_target = logisticFunc(seq(1,10,length.out=10), start=0, end=4.332e5); annual_project_target[11:20] <- 4.332e5

#########################################
## Run FORSYS W/ FIRE                  ##
#########################################

run('config_TenYearPlan_WW_FS.R', fire_dynamic_forsys = TRUE, write_tags = '_Dyn_3x', fire_intersect_table = f_df %>% filter(FUTURE == 10))
run('config_TenYearPlan_WW_FS.R', fire_dynamic_forsys = TRUE, fire_intersect_table = f_df %>% filter(DECILE == 5, FUTURE == 1))

# run multiple scenarios using purrr ...

fire_scn_inputs = expand.grid(DECILE = 5:9, FUTURE = 1:2)

fire_scn_inputs %>% pmap(function(...){
  scn_i = tibble(...)
  fires_i <- fread(input_stand_fire_intersect) %>% right_join(scn_i, by = names(scn_i)) %>% dplyr::select(-n, -PA_ID, -OWNER, -AREA_HA)
  run(config_file = 'config_TenYearPlan_WW_FS.R', write_tags='_Dyn4', fire_dynamic_forsys = TRUE, fire_random_projects = FALSE,
      fire_intersect_table = fires_i, ...)
})

#########################################
## Post-process FORSYS W/ FIRE outputs ##
#########################################

# combine stand level outputs & tag w/ forsys type
process_outputs('config_TenYearPlan_WW_FS.R',
                project_tag = 'proj',
                stand_tag = 'stnd',
                write_tags='',
                fires_dat = fread(input_stand_fire_intersect) %>%
                  dplyr::select(-n, -PA_ID, -OWNER, -AREA_HA) %>%
                  filter(FUTURE %in% c(1:10)))

#########################################
## Animate FORSYS W/ FIRE outputs     ##
#########################################

# see R/fire_misc_animations.R
tmp <- fread('output/WW_10yr_FS_80p/_stnd_combined.csv')
tmp %>% group_by(DECILE, FUTURE) %>% summarize(AREA_HA = sum(AREA_HA, na.rm=T))



