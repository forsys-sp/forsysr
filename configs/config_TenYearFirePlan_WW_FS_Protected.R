###############################
### ForSys Input Parameters ###
###       04/15/2021        ###
###   FS Lands - West       ###
###   Conifer Stands Only   ###
###                         ###
###############################

scenario_name <- "WW_10yr_FS_Protected"
scenario_stand_filename <- c('~/Dropbox/!!projects/aa_10yr/10yr_protected/data/hex_data_protected.csv')
stand_id_field = 'CELL_ID'
stand_filter <- "OwnerCat == 'USFS' & Conif_Maj == 1 & Manage == 0 & AllDisturb_flg == 0"
pcp_spm <- c("aTR_MS",'FireDef')
stand_pcp_spm <- c("FireDef",'aTR_MS')
scenario_priorities <- c('FireDef_SPM')
proj_id <- "PA_ID"
proj_fixed_target <- FALSE
proj_target_field <- "FireDef"
proj_target_multiplier <- 0.8
proj_target_value <- proj_target_multiplier
scenario_weighting_values <- c("1 1 1")
proj_thresholds <- c("RxFire: FRG <= 3")
scenario_output_fields <- c("AREA_HA", "aTR_MS", "aTR_MS_PCP", "FireDef", "FireDef_PCP")
scenario_output_grouping_fields <- c("PA_ID", "FSHED_ID")

logisticFunc = function(yr, mid = 5, normalize = T, start = 0, end = 1){
  y <- 1/(1+(exp(-1 * (yr - mid))))
  if(normalize) y <- (y-min(y))/(max(y) - min(y))
  y = start + (y * abs(start - end))
  return(y)
}

#########################################
## Parameters for FORSYS W/ FIRE       ##
#########################################

# fire_dynamic_forsys = FALSE
# fire_dynamic_forsys = TRUE
# fire_intersect_table <- 'data/hexnet_west_fsim19_30reps_intersect.csv'
# fire_annual_target = 450000 # # 1.1 M Acres treated per year
fire_planning_years = 20
fire_annual_target_field = 'ETrt_AREA_HA'
max_rx_rate = 5e5 # maximum 1.2 million ha per year
fire_annual_target = max_rx_rate * logisticFunc(yr = 1:10, mid = 5, normalize = T)
write_stand_outputs <- TRUE

