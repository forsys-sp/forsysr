########################################################################
##                                                                    ##
##   ForSysR: The R implementation of ForSys scenario planning model  ##
##   Author: Rachel Houtman, Oregon State University                  ##
##   Origination date: 02/16/2018                                     ##
##   Last updated: 10/01/2020                                         ##
##                                                                    ##
########################################################################



#' Run the ForSys treatment planner. Either provide parameters, or define parameters
#' in a config file and pass the name of the file to this run function.
#'
#' @param config_file Relative path to a config file that defines needed parameters
#' @param scenario_name A name for this scenario
#' @param input_standfile TODO
#' @param write_stand_outputs TODO
#' @param stand_field TODO
#' @param pcp_spm PCP and SPM values will be calculated for these variables. This should include the priorities and any value outputs.
#' @param land_base The land base is the area that is used to calculate the PCP and SPM values.
#'                  It is currently a single, binary variable that must be computed prior to running the ForSysR script.
#'                  A blank field means all lands are included in the calculation.
#' @param priorities Priorities are named here. If only one priority exists, only a weight of one will be used.
#' @param stand_group_by TODO
#' @param pa_target TODO
#' @param pa_unit TODO
#' @param pa_target_multiplier TODO
#' @param nesting TODO
#' @param nesting_group_by TODO
#' @param nesting_target TODO
#' @param nesting_target_multiplier TODO
#' @param weighting_values Defines the weights and integer steps between weights. The values are for min, max, and step.
#' @param thresholds Thresholds are defined by type (the first value in the string). The current code only uses one type (Commercial).
#' @param include_stands This defines global threshold values to include stands - i.e. for any threshold type.
#' @param output_fields This should include the desired fields for the planning area treatment files. Planning area id,
#'                      priority weights and treatment rank are added automatically.
#' @param grouping_variables Include the smaller and larger groups here for grouping of treated stands.
#' @param fixed_target Set to have either a fixed area target (TRUE) or a variable area target (FALSE)
#' @param fixed_area_target TODO
#' @param overwrite_output Toggle to overwrite existing output files
#' @param run_with_shiny Sets some output business for better shiny interaction
#' @return A datatable with the weighted values for the priorities in the \code{priorityList}.
#' @export
run <- function(
  config_file = '',
  scenario_name = '',
  input_standfile = '',
  write_stand_outputs = FALSE,
  stand_field = 'Cell_ID',
  pcp_spm = c(),
  land_base = '',
  priorities = c(),
  stand_group_by = '',
  pa_target = '',
  pa_unit = '',
  pa_target_multiplier = 0.15,
  nesting = FALSE,
  nesting_group_by = NULL,
  nesting_target = NULL,
  nesting_unit = NULL,
  nesting_target_multiplier = 1.0,
  weighting_values = "0 5 1",
  thresholds = c("Manageable man_alldis == 1") ,
  include_stands = c("man_alldis == 1"),
  output_fields = c("AREA_HA", "TVMBF_STND", "TVMBF_PCP", "HUSUM_STND", "HUSUM_PCP"),
  grouping_variables = c("PA_ID", "Owner"),
  fixed_target = FALSE,
  fixed_area_target = 2000,
  overwrite_output = TRUE,
  run_with_shiny = FALSE
  ) {

  # If a config file has been selected, source it to read in variables
  if (length(config_file) > 0) {
    configuration_file <- config_file
    setwd(dirname(configuration_file))
    source(configuration_file, local = TRUE)
  } else {

  }

  source('R/forsys_libraries.R')
  source('R/forsys_functions.R')

  ## Load functions, write parameter data out to Arc.
  options(scipen = 9999)

  relative_output_path = glue('output/{scenario_name}')

# Check if output directory exists
absolute_output_path = file.path(getwd(), relative_output_path)
if (!dir.exists(absolute_output_path)) {
  if (run_with_shiny) {

  } else {
    print(paste0("Making output directory: ", absolute_output_path))
  }
  dir.create(absolute_output_path, recursive=TRUE)
} else {
  if (run_with_shiny) {

  } else {
    print(paste0("output directory, ", absolute_output_path, ", already exists"))
  }
}

  if (overwrite_output) {
    ## Clean up any files left from previous database. Failure to remove the .ini file will cause failures when
    ## table attributes change.

    # These paths or wildcards don't seem to match any more
    unlink("output/*.csv")
    unlink("output/*.ini")

    output_files <- sapply(list.files(relative_output_path), function(x) glue('{relative_output_path}/{x}'))
    if (length(output_files) > 0) { file.remove(output_files) }
  } else {
    fname <- paste0(relative_output_path, '/pa_all_', scenario_name, '.csv')
    if (file.exists(fname)) {
      print(paste0('Warning: Output file ', fname, ' already exists. Appending results.'))
    }
  }

  ## Print a specs document that describes the inputs.
  #printSpecsDocument(constraints[[1]][2], priorities, timber_threshold, volume_constraint)

  #
  # # # Load data -------------
  standDT <- load_dataset(input_standfile)
  print("data loaded")
  standDT %>%
    calculate_spm_pcp(land_base, pcp_spm) %>%
    # Add target area or volume fields based on a land base here:
    add_target_field(pa_unit, pa_target, pa_target_multiplier, stand_group_by, land_base)
  print("target added")

  ## Maintain the original stand list. allStands becomes the filtered table.
  allStands <- standDT %>%
    # Hack the area target - can be set in the shapefile.
    # This code may be updated for looping multiple treatment types.
    # It should do the same thing that Pedro is working on within a single run instead of wrapped.
    set_up_treatment_types(allStands)

  # Set adjacencies for spatial optimization
  if(spatial_optimization == TRUE){
    if(calculate_adjacency_list == TRUE){
      print("Calculating adjacency file")
      adjacency_list <- calculate_adj(Shapefile = paste0(getwd(), stand_shapefile), Adjdist = 3)
      if(save_adjacency == TRUE){
        adj_list <- as.data.frame(get.edgelist(adjacency_list))
        fwrite(adj_list, paste0(getwd(), adjacency_pathway))
      }
    }else{
      print("Loading adjacency file")
      adjacency_list <- read_adj(fread(paste0(getwd(),(existing_adjacency))))
    }
  }

  # Calculate weights
  weights <- weight_priorities(length(priorities), weighting_values[1])
  print(paste0("These parameters have defined ", nrow(weights), " weighted scenarios. Running now."))

  # Run selection code for each set of weights
  for (w in 1:nrow(weights)) { # START FOR 0

  ## Step 0: create the weighted priorities.
  if (run_with_shiny) {

  } else {
    print(paste0("Creating weighted priorities:",  w, " of ", nrow(weights)))
  }

  allStands$weightedPriority <- 0
  allStands$treat <- 0
  selected_stands <- NULL

  allStands <- set_up_priorities(w, priorities, weights, allStands)
    ## Step 1: Select stands in each planning area based on stand conditions and priorities:
    # Filter dataset using threshold information.

    # TODO Can we make all_thresholds a single statement, and then adjust treatment_types accordingly?
    all_thresholds <- make_thresholds(thresholds)
    treatment_types <- unique(sapply(all_thresholds, function(x) x[1]))
    all_thresholds <- data.table(matrix(unlist(all_thresholds), nrow=length(all_thresholds), byrow=T))
    stands_updated <- allStands[, treatedPAArea := 0]

    # TODO Do we have to do parse/eval?
    # Remove excluded stands (man_alldis == 0, etc.)
    #if(length(include_stands) > 0){
    #  for(f in 1:length(include_stands)){ # START FOR 3
    #    stands_updated <- subset(stands_updated, eval(parse(text = include_stands[f])))
    #  } # STOP FOR 3
    #}

    if(spatial_optimization == FALSE){
      selected_stands <- apply_treatment(treatment_types = treatment_types,
                                         stands = stands_updated,
                                         all_thresholds = all_thresholds,
                                         stand_group_by = stand_group_by,
                                         stand_field = stand_field,
                                         fixed_target = fixed_target,
                                         fixed_area_target = fixed_area_target,
                                         pa_unit = pa_unit,
                                         pa_target = pa_target,
                                         pa_target_multiplier = pa_target_multiplier)

      # # Step 2: IF NESTING: Group the selected subunits by planning area (based on areas previously selected for treatment)

      # Step 3: Identify the best planning areas within each nest.
      if(isTRUE(nesting)) {
        print("Creating Grouped Dataset")
        # Dynamic output variable names
        output_fields <- as.character(output_fields)
        output_grouped_variables <- c(output_fields, nesting_target, "weightedPriority")

        groupedByPA <- create_grouped_dataset(selected_stands,
                                              stand_group_by,
                                              output_grouped_variables)
        if(length(grouping_variables > 1)) {
          groupedByAll <- create_grouped_dataset(selected_stands,
                                                 grouping_variables,
                                                 output_grouped_variables)
        }
        paSubunits = identify_nested_planning_areas(groupedByPA)
      }
      else{
        print("Creating Grouped Dataset")
        # Dynamic output variable names
        output_fields <- as.character(output_fields)
        output_grouped_variables <- c(output_fields, "weightedPriority")

        groupedByPA <- create_grouped_dataset(selected_stands,
                                              stand_group_by,
                                              output_grouped_variables)
        if(length(grouping_variables > 1)) {
          groupedByAll <- create_grouped_dataset(selected_stands,
                                                 grouping_variables,
                                                 output_grouped_variables)
        }
      }

      planning_areas <- data.table(groupedByPA %>% summarize(across(output_fields, sum, .names = "ETrt_{.col}" )))

      uniqueWeights <- ""
      uniqueWeights <- paste0(sapply(1:ncol(weights), function(i) {uniqueWeights <- paste0(uniqueWeights, "_", weights[[i]][w])}), collapse='')

      print("Producing output files for stands and planning areas")
      if(write_stand_outputs == TRUE) {
        write_stand_outputs_to_file(relative_output_path, uniqueWeights, scenario_name, selected_stands)
      }

      planningAreaOutputFile <- paste0(relative_output_path, "/pa_", uniqueWeights, ".csv")

      # compile all planning areas and stands
      allPlanningAreas <- compile_planning_areas_and_stands(uniqueWeights, standDT, stand_group_by, output_fields)
      #Test outputs
      #fwrite(allPlanningAreas, file = "output/allPlanningAreas.csv", sep = ",", row.names = FALSE)

      # compile all planning areas, broken out by any subunits
      allPlanningAreasSubset <- compile_planning_areas_and_stands(uniqueWeights, standDT, grouping_variables, output_fields)

      #Test outputs
      #fwrite(allPlanningAreasSubset, file = "output/allPlanningAreasSubset.csv", sep = ",", row.names = FALSE)

      paOutput <- as.data.table(merge(allPlanningAreas, groupedByPA, by=c(stand_group_by), all.x = TRUE))

      for (i in 1:ncol(weights)) { # START FOR 6
        priorityName <- paste0("Pr_", i, "_", priorities[[i]][1])
        paOutput[, (priorityName) := weights[[i]][w]]
      } # END FOR 6

      paOutput <- paOutput[order(-paOutput$weightedPriority),]
      print("adding treatment rank")
      paOutput[weightedPriority > 0,"treatment_rank" := seq(1:nrow(paOutput[weightedPriority >0,])),]
      paOutput[weightedPriority > 0, treatment_rank_random := sample(treatment_rank)]
      paOutput[is.na(paOutput)] <- 0

      paOutput <- as.data.table(paOutput)
      # TO DO: Why am I getting identical rows of data outputs? Hack: only export unique rows.
      paOutput <- unique(paOutput)
      print("Adding results to master planning area file")
      masterPA = paste0(relative_output_path, "/pa_all_", scenario_name, ".csv")
      if(file.exists(masterPA)) {
        fwrite(paOutput, file = masterPA, sep = ",", row.names = FALSE, append = TRUE, col.names = FALSE)
      } else {
        fwrite(paOutput, file = masterPA, sep = ",", row.names = FALSE)
      }

      ## This code creates outputs for datasets that have subcategories within planning areas, such as ownership. It
      ## produces a single row for each planning area/subset combination.
      if(length(grouping_variables) > 1) {
        # Rename treatment variables and merge into the planning area dataset for output.
        paSubOutput <- merge(allPlanningAreasSubset, groupedByAll, by=c(grouping_variables), all.x = TRUE)
        paSubOutput[is.na(paSubOutput)] <- 0
        paSubOutput <- paSubOutput[order(-paSubOutput$weightedPriority),]
        print("adding treatment rank")
        paSubOutput <- merge(paSubOutput, paOutput[, c(stand_group_by, "treatment_rank", "treatment_rank_random"), with = FALSE], by = c(stand_group_by))
        paSubOutput <- unique(paSubOutput)

        for (i in 1:ncol(weights)) { # START FOR 7
          priorityName <- paste0("Pr_", i, "_", priorities[[i]][1])
          paSubOutput[, (priorityName) := weights[[i]][w]]
        } # END FOR 7


        print("Adding results to the master planning area subset file.")
        subPA = paste0(relative_output_path, "/pa_subset_", scenario_name, ".csv")
        if (file.exists(subPA)) {
          fwrite(paSubOutput, file = subPA, sep = ",", row.names = FALSE, append = TRUE, col.names = FALSE)
        } else {
          fwrite(paSubOutput, file = subPA, sep = ",", row.names = FALSE)
        }
      }
    }
    else{
      ## Combine thresholds and availability fields for Patchmax
      print("Incorporating thresholds and exclusions")
      stand_flag(stands_updated, all_thresholds, "t_and_e")
      print("Simulating projects")
      select_stands <- simulate_projects(St_id = stands_updated[, get(stand_field)],
                                         St_adj = adjacency_list,
                                         St_area = stands_updated[, get(pa_unit)],
                                         St_objective = stands_updated[, weightedPriority],
                                         St_threshold = stands_updated[, t_and_e],
                                         P_size = project_size,
                                         P_number = project_number,
                                         St_threshold_value = .9,
                                         P_size_slack = 5.0,
                                         P_constraint = stands_updated[, HaulCost],
                                         P_constraint_value = 10000)

      # Post processing outputs
      # Join the input data to the output stand data
      setkeyv(stands_updated, stand_field)
      setkey(select_stands[[2]], Stands)
      full_stand_table <- stands_updated[select_stands[[2]]]
      # Create a grouped dataset for summary by project:
      print("Creating Grouped Dataset")
      groupedByPA <- create_grouped_dataset(full_stand_table,
                                            "Project",
                                            output_grouped_variables, "DoTreat")
      groupedByPA <- data.table(groupedByPA %>% summarize(across(output_fields, sum, .names = "ETrt_{.col}" )))
      projectOutput <- groupedByPA

      # Add the weights to track multi-weight scenarios
      weight_scenario <- ""
      for (i in 1:ncol(weights)) { # START FOR 6
        priorityName <- paste0("Pr_", i, "_", priorities[[i]][1])
        select_stands[[2]][, (priorityName) := weights[[i]][w]]
        projectOutput[, (priorityName) := weights [[i]][w]]
      }

      print("Adding results to the stand output file.")
      spatial_optimal = paste0(relative_output_path, "/opt_stands_", scenario_name, ".csv")
      if (file.exists(spatial_optimal)) {
        fwrite(select_stands[[2]], file = spatial_optimal, sep = ",", row.names = FALSE, append = TRUE, col.names = FALSE)
      } else {
        fwrite(select_stands[[2]], file = spatial_optimal, sep = ",", row.names = FALSE)
      }

      print("Adding results to the project output file.")
      spatial_optimal = paste0(relative_output_path, "/opt_projects_", scenario_name, ".csv")
      if (file.exists(spatial_optimal)) {
        fwrite(projectOutput, file = spatial_optimal, sep = ",", row.names = FALSE, append = TRUE, col.names = FALSE)
      } else {
        fwrite(projectOutput, file = spatial_optimal, sep = ",", row.names = FALSE)
      }
    }
  }
}

