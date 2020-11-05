###############################
### ForSys Input Parameters ###
###       8/13/2020         ###
###############################

# This input file defines a single scenario for the Idaho dataset.


scenario_name <- "_Idaho_test"

## Stand layer
is_dbf <- TRUE
is_csv <- FALSE
input_standfile <- "data/IDHexnet_North20190523_Final.dbf"
write_stand_outputs <- FALSE

## Stand field
stand <- "Cell_ID"

## PCP and SPM values will be calculated for these variables. This should include the priorities and any value outputs.
pcp_spm <- c("HUSUM_STND", "TVMBF_STND")

## The land base is the area that is used to calculate the PCP and SPM values. 
## It is currently a single, binary variable that must be computed prior to running the ForSysR script.
## A blank field means all lands are included in the calculation.
land_base <- "man_alldis"

## Priorities are named here. If only one priority exists, only a weight of one will be used.
priorities <- c("HUSUM_STND_SPM", "TVMBF_STND_SPM")


## Area-level constraints. Currently this system can handle a two-step constraint system. The first
## constraint is typically planning areas (PA_ID or PA_ID_New), the second constraint may be forest,
## ownership, region, or system-wide.
##'Area' = telling R you want to use a target type of area
## If you want to treat a set number of hectares, leave this line of code; it will be set below
## FIELDS BELOW ARE REQUIRED ##
## Set the constraint variables:
stand_group_by <- "PA_ID"
pa_target <- "AREA_MAN"
pa_unit <- "AREA_HA"
pa_target_multiplier <- 0.15

# Set for nesting == TRUE, no nesting == FALSE
nesting <- FALSE
nesting_group_by <- NULL
nesting_target <- NULL
nesting_unit <- NULL
nesting_target_multiplier <- 1.0

## Defines the weights and integer steps between weights. The values are for min, max, and step.
weighting_values <- "0 5 1"

## Thresholds are defined by type (the first value in the string). The current code only uses one type (Commercial).
thresholds <- c("Manageable man_alldis == 1") 

## This defines global threshold values to include stands - i.e. for any threshold type.
include_stands <- c("man_alldis == 1")#You could put timber under constraints, and exclude unmanageble here

## This should include the desired fields for the planning area treatment files. Planning area id, 
## priority weights and treatment rank are added automatically.
output_fields <- c("AREA_HA", "TVMBF_STND", "TVMBF_PCP", "HUSUM_STND", "HUSUM_PCP")

## Include the smaller and larger groups here for grouping of treated stands.
grouping_variables <- c("PA_ID", "Owner")

## Set to have either a fixed area target (TRUE) or a variable area target (FALSE)
fixed_target <- FALSE
fixed_area_target <- 2000

## If the constraint is by master nesting unit (i.e. treat the top X planning areas in each 
## national forest), set FALSE. If the constraint is by the system (i.e. go to the best planning 
##area regardless of where it is located), set TRUE.
system_constraint <- FALSE

## Toggle to overwrite existing output files
overwrite_output <- TRUE