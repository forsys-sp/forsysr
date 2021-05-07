###############################
### ForSys Input Parameters ###
###       04/15/2021        ###
###   FS Lands - West       ###
###   Conifer Stands Only   ###
###                         ###
###############################

# This configuration file includes the parameters for an all-lands, western regions scenario.
# The treateable landbase is
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

scenario_name <- "WW_10yr_FS_80p"
num_reps <- 30

## Stand layer
input_standfile <- c("../../Dropbox/!!projects/!archive/aa_10yr/data/Hexnet_WestFS.csv")
writeStandOutputs <- TRUE

# Create pcp and spm values for these fields.
pcp_spm <- c("aTR_MS","F2F2_IMP", "TVMBF_STND", "HF_Cost", "Value_MBF", "BurnedSPAHaYr")

# Creating updated values for target area based on different land bases (binary field where 1 == include)
land_base <- "western_flag20_NLCD"

## Stand field
stand_field <- "CELL_ID"

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
pa_target <- "aTR_MS_Tot"
pa_unit <- "aTR_MS"
pa_target_multiplier <- 0.8

# Set for nesting == TRUE, no nesting == FALSE
nesting <- FALSE
nesting_group_by <- "FORESTORGCODE"
nesting_target <- "ForestTarget"
nesting_unit <- "AREA_HA"
nesting_target_multiplier <- 0.5

## Defines the weights and integer steps between weights. The values are for min, max, and step.
weighting_values <- c("1 1 1")
# weighting_values <- c("0 1 1")


## Thresholds are defined by type (the first value in the string). The current code only uses one type (Commercial).
thresholds <- c("Commercial western_flag20_NLCD == 1")
#thresholds <- c("Commercial Manage_new > 1;Commercial TVMBF_STND > 1")#Example with two constraints

## This defines global threshold values to include stands - i.e. for any threshold type.
include_stands <- c("aTR_MS > 0")#You could put timber under constraints, and exclude unmanageble here
#"Conif_Maj == 1", "forest_fla == 1",
# Echo fields are values that should be included *without* aggregation.
#echo_fields <- c("FSHED_ID")

## This should include the desired fields for the planning area treatment files. Planning area id,
## priority weights and treatment rank are added automatically.
output_fields <- c("AREA_HA", "aTR_MS", "aTR_MS_PCP", "TVMBF_STND", "TVMBF_STND_PCP", "TVMBF_STND_SPM", "Standing_MBF",
                  "HF_Cost", "HF_Cost_PCP", "Value_MBF", "Value_MBF_PCP", "F2F2_IMP", "F2F2_IMP_PCP",
                  "BurnedSPAHaYr", "BurnedSPAHaYr_PCP")

## Include the smaller and larger groups here for grouping of treated stands.
grouping_variables <- c("PA_ID", "FSHED_ID", "MajNF_PA")

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

# dynamic_forsys = TRUE
input_stand_fire_intersect <- '../../Dropbox/!!projects/aa_10yr_uncertainity/WW_HexID_FSIM19_30reps_allLands.csv'
annual_project_target = 450000 # # 1.1 M Acres treated per year
planning_years = 20

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

