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
    num_reps = 1,
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

    set.seed(1)

    # If a config file has been selected, source it to read in variables
    if (length(config_file) > 0) {
      configuration_file <- config_file
      setwd(dirname(configuration_file))
      source(configuration_file, local = TRUE)
    } else {

    }

    source('R/forsys_libraries.R')
    source('R/forsys_functions.R')

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
      ## Clean up any files left from previous database.

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

    # !!!!!!!!!!!!!!!!!!!!!!!!!!!
    # !!!!! 1. PREP STANDS !!!!!!
    # !!!!!!!!!!!!!!!!!!!!!!!!!!!

    # # # Load data -------------
    stands <- load_dataset(input_standfile)

    # Calculate SPM & PCP values
    stands <- stands %>%
      calculate_spm_pcp(filter = land_base,
                        fields = pcp_spm)

    # Add target area or volume fields based on a land base here:
    stands <- stands %>%
      add_target_field(pa_unit = pa_unit,
                       pa_target = pa_target,
                       pa_target_multiplier = pa_target_multiplier,
                       stand_group_by = stand_group_by,
                       land_base = land_base)

    # Read fire-stand intersect data
    f_df <- fread(input_stand_fire_intersect)

    # set up weighting scenarios
    weights <- weight_priorities(numPriorities = length(priorities), weights = weighting_values[1])

    # Run selection code for each set of weights
    for (w in 1:nrow(weights)) { # START WEIGHTING INSTANCE

      ## Step 0: create the weighted priorities.
      if (run_with_shiny) {
      } else {
        print(paste0("Creating weighted priorities:",  w, " of ", nrow(weights)))
      }

      # prep stand data
      stands_all <- stands %>%
        set_up_treatment_types() %>%
        set_up_priorities(w = w,
                          priorities = priorities,
                          weights = weights)

      if(random_stands){
        warning('!! Stand weights were randomized')
        scenario_name <- paste0(scenario_name, '_srand')
        stands_all$weightedPriority <- sample(stands_all$weightedPriority, nrow(stands_all))
      }

      stands_select <- NULL
      stands_updated <- stands_all[, treatedPAArea := 0]

      # !!!!!!!!!!!!!!!!!!!!!!!!!!!
      # !!!! 2. SELECT STANDS !!!!!
      # !!!!!!!!!!!!!!!!!!!!!!!!!!!

      # specify thresholds
      thresholds <- make_thresholds(thresholds = thresholds)

      # select stands from project areas until target reached while filtering by threshold
      stands_select <- stands_updated %>%
        apply_treatment(
          treatment_types = thresholds$types,
          all_thresholds = thresholds$thresholds,
          stand_group_by = stand_group_by,
          stand_field = stand_field,
          fixed_target = fixed_target,
          fixed_area_target = fixed_area_target,
          pa_unit = pa_unit,
          pa_target = pa_target,
          pa_target_multiplier = pa_target_multiplier
        )

      # !!!!!!!!!!!!!!!!!!!!!!!!!!!
      # !!! 3. RANK PROJECTS !!!!!!
      # !!!!!!!!!!!!!!!!!!!!!!!!!!!

      # group selected stands by project
      etrt_by_pa <- stands_select %>%
        create_grouped_dataset(grouping_vars = stand_group_by,
                               summing_vars = c(output_fields, "weightedPriority")) %>%
        rename_with(.fn = ~ paste0("ETrt_", .x), .cols = output_fields)

      # group all candidate stands by project
      esum_by_pa <- stands %>%
        compile_planning_areas_and_stands(unique_weights = uniqueWeights,
                                          group_by = stand_group_by,
                                          output_fields = output_fields)

      # combine etrt and esum
      projects <- esum_by_pa %>%
        left_join(etrt_by_pa, by=stand_group_by) %>%
        replace(is.na(.), 0) %>%
        arrange(-weightedPriority) %>%
        mutate(treatment_rank = ifelse(weightedPriority > 0, 1:n(), 0))

      if(random_projects){
        warning('!! Project weights were randomized')
        scenario_name <- paste0(scenario_name, '_prand')
        projects <- projects %>%
          mutate(weightedPriority = sample(weightedPriority)) %>%
          arrange(-weightedPriority) %>%
          mutate(treatment_rank = ifelse(weightedPriority > 0, 1:n(), 0))
      }

      # !!!!!!!!!!!!!!!!!!!!!!!!!!!
      # !!! 4. SCHEDULE PROJECTS !!
      # !!!!!!!!!!!!!!!!!!!!!!!!!!!


      # !!!!!!!!!!!!!!!!!!!!!!!!!!!
      # !!! 5. DETECT EVENTS !!!!!!
      # !!!!!!!!!!!!!!!!!!!!!!!!!!!


      # !!!!!!!!!!!!!!!!!!!!!!!!!!!
      # !!!!! 4. WRITE DATA !!!!!!!
      # !!!!!!!!!!!!!!!!!!!!!!!!!!!

      # WRITE: write stands to file ---------------

      print("Producing output files for stands and planning areas")
      if(write_stand_outputs) {
        stand_fields_to_write = c(stand_field, stand_group_by, output_fields)
        write_stand_outputs_to_file(dir = relative_output_path,
                                    unique_weights = paste0(weights[w,], collapse = '_'),
                                    name = scenario_name,
                                    selected_stands = stands_select,
                                    write_fields = stand_fields_to_write)
      }

      # WRITE: write project to file ---------------

      masterPA = paste0(relative_output_path, "/proj_", scenario_name, ".csv")
      projects[,paste0('Pr_', 1:length(priorities), '_', priorities)] = weights[1,]
      if(file.exists(masterPA)) {
        fwrite(projects, file = masterPA, sep = ",", row.names = FALSE, append = TRUE, col.names = FALSE)
      } else {
        fwrite(projects, file = masterPA, sep = ",", row.names = FALSE)
      }

      } # END WEIGHT LOOP
  }

