########################################################################
##                                                                    ##
##   R-LTD: The R implementation of the Landscape Treatment Designer  ##
##   Author: Rachel Houtman, Oregon State University                  ##
##   Origination date: 02/16/2018                                     ##
##   Last updated: 08/14/2020                                         ##
##                                                                    ##
########################################################################

## Set parameters here.
## configuration_file is a relic - currently used to set working directory.
configuration_file <- c("C:/Users/Houtmanr/Desktop/ForSysR_2.0/config_Idaho.R")

#-----------------------------------------------------------------

setwd(dirname(configuration_file))
# Check if output directory exists
if(!dir.exists(file.path(getwd(), "output"))){
  print(paste("Making output directory: ", file.path(getwd(), "output")), sep="")
  dir.create(file.path(getwd(), "output"))
} else(
  print(paste("output directory, ", file.path(getwd(), "output"), ", already exists"), sep="")
)
  
## Clean up any files left from previous database. Failure to remove the .ini file will cause failures when 
## table attributes change. 
##unlink("output\\*.csv")
##unlink("output\\*.ini")

## Print a specs document that describes the inputs.
#printSpecsDocument(constraints[[1]][2], priorities, timber_threshold, volume_constraint)

## Load functions, write parameter data out to Arc.
source(configuration_file)
source("R/forsys_functions.R")
options(scipen = 9999)
('Loading required R packages...')
#install.packages('pacman')
pacman::p_load(dplyr, data.table, rgdal, ggplot2, sp, grid, maptools, rgeos, ggsn, roxygen2, foreign, gtools, hexbin, stringr)
  
#
# # # Load data -------------
print("Loading Dataset")
if(is_dbf == TRUE){
  standDT <- data.table(read.dbf(input_standfile))
}else if(is_csv == TRUE){
  standDT <- data.table(fread(input_standfile, header = TRUE))
}else{
  ('Input format not recognized')
}

## Add in SPM and PCP values based on the specified land base ##
standDT <- calculateSPMPCP(standDT, land_base, pcp_spm)

# Add target area or volume fields based on a land base here:
standDT <- addTargetField(standDT, pa_unit, pa_target, pa_target_multiplier, subunit_group_by, land_base)

# Dynamic output variable names
output_fields <- as.character(output_fields)
output_grouped_variables <- c(output_fields,
                              pa_target, "weightedPriority")
    
# File with constraints by forest # 
#CURRENTLY DEFUNCT between hash lines - CAN BE REVIVED when forest/fireshed/etc. constraints are needed. #
##################################
#print("Loading forest-level data table")
#forestConstraintsFile <- data.table(read.csv(constraint_file))

## TO DO: set up a file with standard forest-level constraint column names
#allStands <- merge(standDT, forestConstraintsFile[, c("FORESTID", "HISTCON_CCF", "MechTrt_ratio", "RxTrt_ratio", "HarvestTrt_PerTot", "MechTrt_PerTot", "RxTrt_PerTot")], by="FORESTID", all.x=TRUE)
##################################

allStands <- standDT

# Hack the area target - can be set in the shapefile. 
# This code may be updated for looping multiple treatment types.
# It should do the same thing that Pedro is working on within a single run instead of wrapped.
allStands$Commercial <- allStands$Commercial*allStands$AREA_HA
allStands$PreCommercial <- allStands$AREA_HA - allStands$Commercial
allStands$treat <- 0
allStands$treatment_type <- ""
allStands$treatedPAArea <- 0

# Calculate weights
weights <- weightPriorities(length(priorities), weighting_values[1])
print(paste0("These parameters have defined ", nrow(weights), " weighted scenarios. Running now."))

# Run selection code for each set of weights
for(w in 1:nrow(weights)){
  ## Step 0: create the weighted priorities.
  print(paste0("Creating weighted priorities:",  w, " of ", nrow(weights)))

  allStands$weightedPriority <- 0
  allStands$treat <- 0
  selectedStands <- NULL
  for(i in 1:ncol(weights)){
    allStands$weightedPriority <- allStands$weightedPriority + weights[[i]][w]*allStands[, get(priorities[[i]][1])]
    priorityName <- paste0("Pr_", i, "_", priorities[[i]][1])
    allStands[, (priorityName) := weights[[i]][w]]
  }
  ## Step 1: Select stands in each planning area based on stand conditions and priorities:
  # Filter dataset using threshold information
  # Convert threshold strings into a data table to produce filters.
  # Remove excluded stands (man_alldis == 0, etc.)
  all_thresholds <- NULL
  for(i in 1:length(as.list(strsplit(thresholds, ";"))[[1]])){
    all_thresholds <- rbind(all_thresholds, strsplit(as.list(strsplit(thresholds, ";"))[[1]][i], " ")[[1]])
  }
  
  treatment_types <- unique(all_thresholds[,1])
  all_thresholds <- as.data.table(all_thresholds)
  standSubunitsUpdated <- allStands[, treatedPAArea := 0]
  for(f in 1:length(include_stands)){
    standSubunitsUpdated <- subset(standSubunitsUpdated, eval(parse(text = include_stands[f])))
  }
  
  for(t in 1:length(treatment_types)){
    filteredStands <- standFilter(standSubunitsUpdated, all_thresholds[V1 == treatment_types[t], ])
    print(paste0("There are ", nrow(filteredStands), " stands that meet treatment thresholds for ", treatment_types[t]))
    # Set the target for each bin here:
    print(paste0("Threshold: ", all_thresholds[t,1]))
    if(fixed_target == TRUE){
      filteredStands[, current_area_target := 2000]#This overwrites the constraint field for percentage of manageable
    }else if(fixed_target == FALSE){
      filteredStands[, current_area_target := get(pa_target)*pa_target_multiplier]#activate for percentage not fixed area
    }
    # The following code splits out the planning area treatments by treatment type and percent of area treated. An area (rather than %)
    # area target can be set by removing the multiplier and replacing AREA_MAN with the area target in hectares (i.e. 2000).
    # if(all_thresholds[t,1] == c("Commercial")){
    #   filteredStands[, current_area_target := AREA_MAN *.15 - treatedPAArea]
    # }else if(all_thresholds[t,1] == c("HF")){
    #   filteredStands[, current_area_target := AREA_MAN * .30 - treatedPAArea]
    #   filteredStands[, weightedPriority := WHPMN_SPM]
    # }else{
    #   print("Missing Threshold Update! Treatment selection is suspect!")
    # }
    standSubunits <- selectSubunits(dt = filteredStands,
                                    grouped_by = subunit_group_by,
                                    prioritize_by = "weightedPriority",
                                    tally_by = pa_unit,
                                    grouped_target = "current_area_target")
    
    # This updates the total area available for activities. Original treatment target - total area treated for each subunit (planning area).
        
    area_treatedPA <- updateTarget(standSubunits, subunit_group_by, pa_unit)
    standSubunitsUpdated <- standSubunitsUpdated[area_treatedPA,  treatedPAArea := treatedPAArea + i.sum, on = subunit_group_by]
    standSubunitsUpdated <- standSubunitsUpdated[standSubunits, ':='(treatment_type = treatment_types[t], treat = 1), on = subunit]
    selectedStands <- rbind(selectedStands, standSubunitsUpdated[treat==1,])
  }
  
  # # Step 2: IF NESTING: Group the selected subunits by planning area (based on areas previously selected for treatment)
  print("Creating Grouped Dataset")
  groupedByPA <- createGroupedDataset(selectedStands,
                                      subunit_group_by,
                                      output_grouped_variables)
  if(length(grouping_variables > 1)){
    groupedByAll <- createGroupedDataset(selectedStands,
                                         grouping_variables,
                                         output_grouped_variables)
  }
  if(isTRUE(nesting)){

    # Step 3: Identify the best planning areas within each nest.
    print("Selecting planning area subunits")
    groupedByPA$system <- 1
    if(system_constraint == TRUE){
      grouping_type <- c("system")
      groupedByPA[,harvestTarg := sum(groupedByPA[,get(nesting_target) * get(nesting_unit)])]

    }else{
      grouping_type <- nesting_unit
      groupedByPA[,harvestTarg := groupedByPA[,get(nesting_target) * get(nesting_unit)]]
    }
    groupedByPA$treat <- 0
    paSubunits <- selectSubunits(dt = groupedByPA,
                                 grouped_by = grouping_type,
                                 prioritize_by = "weightedPriority",
                                 tally_by = nesting_unit,
                                 grouped_target = "harvestTarg")
    paSubunits <- paSubunits[order(-paSubunits$weightedPriority),]
    print("adding treatment rank")
    paSubunits$treatment_rank <- seq(1:nrow(paSubunits))
  }
  
  
  uniqueWeights <- ""
   for(i in 1:ncol(weights)){
     uniqueWeights <- paste0(uniqueWeights, "_", weights[[i]][w])
   }
  print("Producing output files for stands and planning areas")
  standOutputFile <- paste0("output\\stands", uniqueWeights, scenario_name, ".csv")
  #fwrite(selectedStands, standOutputFile)
  planningAreaOutputFile <- paste0("output\\pa", uniqueWeights, ".csv")
  
  # compile all planning areas and stands
  groupAllPlanningAreas <- standDT %>%  
    group_by_at(subunit_group_by) 
  
  allPlanningAreas <- data.table(summarize_at(groupAllPlanningAreas, .vars = vars(output_fields), .funs = c(sum="sum")))
  names(allPlanningAreas) <- gsub(x = names(allPlanningAreas), pattern = "_sum", replacement = "") 
  sum_names <- paste0("ESum_", colnames(allPlanningAreas[,output_fields, with = FALSE]), sep = "")
  setnames(allPlanningAreas, c(output_fields), c(sum_names))
  fwrite(allPlanningAreas, file = "output\\allPlanningAreas.csv", sep = ",", row.names = FALSE)
  
  # compile all planning areas, broken out by any subunits
  groupAllPlanningAreasSubset <- standDT %>% 
    group_by_at(grouping_variables)
  
  allPlanningAreasSubset <- data.table(summarize_at(groupAllPlanningAreasSubset, .vars = vars(output_fields), .funs = c(sum="sum")))
  names(allPlanningAreasSubset) <- gsub(x = names(allPlanningAreasSubset), pattern = "_sum", replacement = "") 
  sum_names <- paste0("ESum_", colnames(allPlanningAreasSubset[,output_fields, with = FALSE]), sep = "")
  setnames(allPlanningAreasSubset, c(output_fields), c(sum_names))
  fwrite(allPlanningAreasSubset, file = "output\\allPlanningAreasSubset.csv", sep = ",", row.names = FALSE)
  
  # Rename treatment variables and merge into the planning area dataset for output.
  trt_names <- paste0("ETrt_", colnames(groupedByPA[,output_fields, with = FALSE]), sep = "")
  setnames(groupedByPA, c(output_fields), c(trt_names))
  paOutput <- merge(allPlanningAreas, groupedByPA, by=c(subunit_group_by), all.x = TRUE)
  for(i in 1:ncol(weights)){
    priorityName <- paste0("Pr_", i, "_", priorities[[i]][1])
    paOutput[, (priorityName) := weights[[i]][w]]
  }
  paOutput <- paOutput[order(-paOutput$weightedPriority),]
  print("adding treatment rank")
  paOutput[,"treatment_rank" := seq(1:nrow(paOutput)),]
  paOutput[is.na(paOutput)] <- 0
  paOutput[weightedPriority == 0, "treatment_rank" := 0] 
  
  paOutput <- as.data.table(paOutput)
  # TO DO: Why am I getting identical rows of data outputs? Hack: only export unique rows.
  paOutput <- unique(paOutput)
  print("Adding results to master planning area file")
  masterPA = paste0("output\\pa_all", scenario_name, ".csv")
  if(file.exists(masterPA)){
    fwrite(paOutput, file = masterPA, sep = ",", row.names = FALSE, append = TRUE, col.names = FALSE)
  }else{
    fwrite(paOutput, file = masterPA, sep = ",", row.names = FALSE)
  }
  
  ## This code creates outputs for datasets that have subcategories within planning areas, such as ownership. It
  ## produces a single row for each planning area/subset combination.
  if(length(grouping_variables > 1)){
    # Rename treatment variables and merge into the planning area dataset for output.
    trt_names <- paste0("ETrt_", colnames(groupedByAll[,output_fields, with = FALSE]), sep = "")
    setnames(groupedByAll, c(output_fields), c(trt_names))
    paSubOutput <- merge(allPlanningAreasSubset, groupedByAll, by=c(grouping_variables), all.x = TRUE)
    paSubOutput[is.na(paSubOutput)] <- 0
    paSubOutput <- paSubOutput[order(-paSubOutput$weightedPriority),]
    print("adding treatment rank")
    paSubOutput <- merge(paSubOutput, paOutput[, c(subunit_group_by, "treatment_rank"), with = FALSE], by = c(subunit_group_by))
    paSubOutput <- unique(paSubOutput)
    for(i in 1:ncol(weights)){
      priorityName <- paste0("Pr_", i, "_", priorities[[i]][1])
      paSubOutput[, (priorityName) := weights[[i]][w]]
    }
    
    
    print("Adding results to the master planning area subset file.")
    subPA = paste0("output\\pa_subset", scenario_name, ".csv")
    if(file.exists(subPA)){
      fwrite(paSubOutput, file = subPA, sep = ",", row.names = FALSE, append = TRUE, col.names = FALSE)
    }else{
      fwrite(paSubOutput, file = subPA, sep = ",", row.names = FALSE)
    }
  }
}


