###############################
### ForSys Input Parameters ###
###       04/15/2021        ###
###   FS Lands - West       ###
###   Conifer Stands Only   ###
###                         ###
###############################

# This configuration file includes the parameters for an all-lands, western regions scenario.
# The treatable landbase is
#   1) manageable lands that have not been disturbed (man_alldis == 1),
#   2) forested lands (forest_flg == 1),
#   3) FVS conifer designations (western_flg == 1)
#
# Treatment threshold: none
# Treatment constraint: none
# Priority: structure exposure (aTR_MS - calculated with a land base of all lands)
# Effects:
#   1) Structure exposure (aTR_MS)
#   2) Costs (HF_Cost)
#   3) Revenue (MBF_VALUE)
#   4) Forest to Faucets (F2F2_Imp)

scenario_name <- "WW_10yr_FS_P_80p"

## Stand layer
# input_standfile <- c("data/hexnet_west_fs.csv")
input_standfile <- c('~/Dropbox/!!projects/aa_10yr/10yr_protected/data/hex_data_protected.csv')
input_filter <- "OwnerCat == 'USFS' & Conif_Maj == 1 & Manage == 0 & AllDisturb_flg == 0"

writeStandOutputs <- TRUE

# Create pcp and spm values for these fields.
pcp_spm <- c("aTR_MS")

# Creating updated values for target area based on different land bases (binary field where 1 == include)
# land_base <- "AllDisturb_flg == 0 & Conif_Maj == 1"

## Stand field
stand_field <- "CELL_ID"
include_stands <- c("aTR_MS > 0")
## This defines global threshold values to include stands - i.e. for any threshold type.
#"Conif_Maj == 1", "forest_fla == 1",
# Echo fields are values that should be included *without* aggregation.
#echo_fields <- c("FSHED_ID")

## Priorities are named here. If only one priority exists, only a weight of one will be used.
priorities <- c("aTR_MS_SPM")
# priorities <- c("aTR_MS_SPM","TVMBF_STND_SPM")

## Area-level constraints. Currently this system can handle a two-step constraint system. The first
## constraint is typically planning areas (PA_ID or PA_ID_New), the second constraint may be forest,
## ownership, region, or system-wide.
##'Area' = telling R you want to use a target type of area
## If you want to treat a set number of hectares, leave this line of code; it will be set below
## FIELDS BELOW ARE REQUIRED ##
## Set the constraint variables:
stand_group_by <- "PA_ID"
proj_target <- "aTR_MS_Tot"
proj_unit <- "aTR_MS"
proj_target_multiplier <- 0.8
# proj_unit * proj_target_multiplier

## Defines the weights and integer steps between weights. The values are for min, max, and step.
weighting_values <- c("1 1 1")

## Thresholds are defined by type (the first value in the string). The current code only uses one type (Commercial).
thresholds <- c("RxFire Manage == 0")
#thresholds <- c("Commercial Manage_new > 1;Commercial TVMBF_STND > 1")#Example with two constraints

## This should include the desired fields for the planning area treatment files. Planning area id,
## priority weights and treatment rank are added automatically.
output_fields <- c("AREA_HA", "aTR_MS", "aTR_MS_PCP")

## Include the smaller and larger groups here for grouping of treated stands.
grouping_variables <- c("PA_ID", "FSHED_ID")

## Set to have either a fixed area target (TRUE) or a variable area target (FALSE)
fixed_subunit_target <- c(FALSE, 2000)
fixed_nest_target <- c(TRUE, 20000)

## If the constraint is by master nesting unit (i.e. treat the top X planning areas in each
## national forest), set FALSE. If the constraint is by the system (i.e. go to the best planning
##area regardless of where it is located), set TRUE.
system_constraint <- FALSE

#########################################
## Parameters for FORSYS W/ FIRE       ##
#########################################

# dynamic_forsys = FALSE
# random_projects = TRUE
# input_stand_fire_intersect <- 'data/hexnet_west_fsim19_30reps_intersect.csv'
# annual_project_target = 450000 # # 1.1 M Acres treated per year
fire_planning_years = 20
fire_annual_target_field = 'ETrt_AREA_HA'

# 10-year ramp (6.6 M ha treated)
max_rx_rate = 1200000 # maximum 1.2 million ha per year
# fire_annual_target = max_rx_rate * logisticFunc(yr = 1:10, mid = 5, normalize = T)

# 20-year plan w/ 10-year ramp-up
fire_annual_target = logisticFunc(seq(1,10,length.out=10), start=0, end=4.332e5); fire_annual_target[11:20] <- 0

#########################################
## Parameters for SPATIAL OPTIMIZATION ##
#########################################

## Optimize vs. preplanned areas, TRUE optitimizes spatially
spatial_optimization <- FALSE

## If computing new adjacency list from the listed shapefile, TRUE, else FALSE,
## If saving adjacency for late use, TRUE
calculate_adjacency_list <- FALSE
stand_shapefile <- ""
save_adjacency <- TRUE
adjacency_pathway <- ""
write_stand_outputs <- TRUE

## If reading an adjacency list, pathway here:
existing_adjacency <- ""

## Project size defined in hectares
project_size <- 1000

## Number of projects
project_number <- 2

