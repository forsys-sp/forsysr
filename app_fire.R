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

input_stand_fire_intersect <- 'data/hexnet_west_fsim19_30reps_intersect.csv'
input_stand <- 'data/hexnet_west_fs.csv'

if(exists('f_df') == FALSE) f_df <- data.table::fread(input_stand_fire_intersect)
if(exists('hex') == FALSE) hex <- fread(input_stand)

planning_years = 20
annual_project_target = logisticFunc(seq(1,10,length.out=10), start=0, end=5.825e5) %>% round;

#########################################
## Run FORSYS W/ FIRE                  ##
#########################################

run('config_TenYearPlan_WW_FS.R', dynamic_forsys = TRUE, fire_scenario = 1)

# run multiple scenarios using purrr ...

1:10 %>% purrr::map(~run('config_TenYearPlan_WW_FS.R', dynamic_forsys = TRUE, fire_scenario = .))

#########################################
## Post-process FORSYS W/ FIRE outputs ##
#########################################

# combine stand level outputs & tag w/ forsys type
process_outputs('config_TenYearPlan_WW_FS.R')

#########################################
## Animate FORSYS W/ FIRE outputs     ##
#########################################

# see R/fire_misc_animations.R


