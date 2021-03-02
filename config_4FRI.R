

###############################
### ForSys Input Parameters ###
###       2/23/2021         ###
###############################

# This input file defines a single scenario for the Idaho dataset.


scenario_name <- "4FRI_test"

## Stand layer
input_standfile <- "data/4FRI_StandData.dbf"
write_stand_outputs <- FALSE

## Stand field
stand_field <- "UniqueID"

## PCP and SPM values will be calculated for these variables. This should include the priorities and any value outputs.
pcp_spm <- c("DACF_STD")

## The land base is the area that is used to calculate the PCP and SPM values.
## It is currently a single, binary variable that must be computed prior to running the ForSysR script.
## A blank field means all lands are included in the calculation.
land_base <- "Trt2"

## Priorities are named here. If only one priority exists, only a weight of one will be used.
priorities <- c( "DACF_STD_SPM")


## Area-level constraints. Currently this system can handle a two-step constraint system. The first
## constraint is typically planning areas (PA_ID or PA_ID_New), the second constraint may be forest,
## ownership, region, or system-wide.
##'Area' = telling R you want to use a target type of area
## If you want to treat a set number of hectares, leave this line of code; it will be set below
## FIELDS BELOW ARE REQUIRED ##
## Set the constraint variables:
stand_group_by <- "Forest"
pa_target <- "AREA_MAN"
pa_unit <- "Area_HA"
pa_target_multiplier <- 0.15

# Set for nesting == TRUE, no nesting == FALSE
nesting <- FALSE
nesting_group_by <- NULL
nesting_target <- NULL
nesting_unit <- NULL
nesting_target_multiplier <- 1.0

## Defines the weights and integer steps between weights. The values are for min, max, and step.
weighting_values <- "1 1 1"

## Thresholds are defined by type (the first value in the string). The current code only uses one type (Commercial).
thresholds <- c("Manageable Trt2 == 1")

## This defines global threshold values to include stands - i.e. for any threshold type.
include_stands <- c("Trt2 == 1", "DACF_STD > 0.7")#You could put timber under constraints, and exclude unmanageble here

## This should include the desired fields for the planning area treatment files. Planning area id,
## priority weights and treatment rank are added automatically.
output_fields <- c("Area_HA", "RVal_STD", "RVal_PCP", "DACF_STD", "DACF_PCP")

## Include the smaller and larger groups here for grouping of treated stands.
grouping_variables <- c("Forest")

## Set to have either a fixed area target (TRUE) or a variable area target (FALSE)
fixed_target <- FALSE
fixed_area_target <- 2000

## If the constraint is by master nesting unit (i.e. treat the top X planning areas in each
## national forest), set FALSE. If the constraint is by the system (i.e. go to the best planning
##area regardless of where it is located), set TRUE.
system_constraint <- FALSE

## Toggle to overwrite existing output files
overwrite_output <- TRUE

#########################################
## Parameters for SPATIAL OPTIMIZATION ##
#########################################

## Optimize vs. preplanned areas, TRUE optitimizes spatially
spatial_optimization <- TRUE

## If computing new adjacency list from the listed shapefile, TRUE, else FALSE,
## If saving adjacency for late use, TRUE
calculate_adjacency_list <- FALSE
stand_shapefile <- "/data/4FRI_1stEIS_ROD_NewpacAndWinslowScenario.shp"
save_adjacency <- TRUE
adjacency_pathway <- "/data/az_Adj.csv"

## If reading an adjacency list, pathway here:
existing_adjacency <- "/data/az_Adj.csv"

## Project size defined in hectares
project_size <- 1000

## Number of projects
project_number <- 5
