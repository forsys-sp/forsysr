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
    run_with_shiny = FALSE,
    dynamic_forsys = FALSE,
    random_projects = FALSE,
    fire_scenario = 1
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

    # !!!!!!!!!!!!!!!!!!!!!!!!!!!
    # !!!!! 1. PREP STANDS !!!!!!
    # !!!!!!!!!!!!!!!!!!!!!!!!!!!

    # # # Load data -------------
    stands <- load_dataset(input_standfile)

    # Read fire-stand intersect data
    f_df <- fread(input_stand_fire_intersect)

    # Calculate SPM & PCP values
    stands <- stands %>%
      calculate_spm_pcp(filter = land_base,
                        fields = pcp_spm) %>%
      add_target_field(pa_unit = pa_unit,
                       pa_target = pa_target,
                       pa_target_multiplier = pa_target_multiplier,
                       stand_group_by = stand_group_by,
                       land_base = land_base)

    # create objects for tracking treated and burnt stands
    stands_treated <- NULL
    stands_burned <- NULL

    # specify thresholds
    threshold_dat <- make_thresholds(thresholds = thresholds)

    # set up weighting scenarios
    weights <- weight_priorities(numPriorities = length(priorities), weights = weighting_values[1])

    # Run selection code for each set of weights
    for (w in 1:nrow(weights)) { # START WEIGHT LOOP

      ## Step 0: create the weighted priorities.
      if (run_with_shiny) {
      } else {
        print(paste0("Creating weighted priorities:",  w, " of ", nrow(weights)))
      }

      # prep stand data
      stands_prioritized <- stands %>%
        set_up_treatment_types() %>%
        set_up_priorities(w = w,
                          priorities = priorities,
                          weights = weights)

      stands_available <- stands_prioritized

      # !!!!!!!!!!!!!!!!!!!!!!!!!!!
      # !!!! 2. SELECT STANDS !!!!!
      # !!!!!!!!!!!!!!!!!!!!!!!!!!!

      # if(dynamic_forsys == TRUE){
      #   print(paste('Running ForSys dynamically over', planning_years, 'years using fire scenairo', fire_scenario))
      # } else{
      #   planning_years = 1 # only need to run forsys 1 loop since no dynamics
      #   print(paste('Running static version of ForSys'))
      # }

      for(yr in 1:planning_years){ # BEGIN YEAR LOOP

        print(paste('Year', yr, '...'))

        # select stands from project areas until target reached while filtering by threshold
        stands_selected <- stands_available %>%
          apply_treatment(
            treatment_types = threshold_dat$types,
            all_thresholds = threshold_dat$thresholds,
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
        projects_selected <- stands_selected %>%
          create_grouped_dataset(grouping_vars = stand_group_by,
                                 summing_vars = c(output_fields, "weightedPriority")) %>%
          rename_with(.fn = ~ paste0("ETrt_", .x), .cols = output_fields) %>%
          replace(is.na(.), 0) %>%
          arrange(-weightedPriority) %>%
          mutate(treatment_rank = ifelse(weightedPriority > 0, 1:n(), NA))

        if(random_projects){
          print('!! Randomizing projects')

          p_weights_shuffled <- projects_selected %>%
            filter(treatment_rank %>% is.na == FALSE) %>%
            mutate(weightedPriority = sample(weightedPriority)) %>%
            dplyr::select(PA_ID, weightedPriority)

          projects_selected <- projects_selected %>% dplyr::select(-weightedPriority) %>%
            left_join(p_weights_shuffled, by = 'PA_ID') %>%
            arrange(-weightedPriority) %>%
            mutate(treatment_rank = ifelse(weightedPriority > 0, 1:n(), NA))
        }

        # !!!!!!!!!!!!!!!!!!!!!!!!!!!
        # !!! 4. DETECT EVENTS !!!!!!
        # !!!!!!!!!!!!!!!!!!!!!!!!!!!

        # if using dynamic forsys, run the selection routine once for each
        # year in order to identify stands for treatment in the current year.
        # remove stands for project areas that were treated and burnt stands
        # from those stands that are available in the subsequent year. if
        # using static forsys, we run only need to run the stand selection
        # routine once, build projects, then schedule these projects by year

          # schedule projects from year yr into the future based on annual constraint
          schedule_w_select <- projects_selected %>%
            mutate(ETrt_YR = cumsum(ETrt_AREA_HA) %/% !!annual_project_target + 1) %>%
            mutate(ETrt_YR = ifelse(weightedPriority == 0, NA, ETrt_YR)) %>%
            dplyr::select(PA_ID, ETrt_YR, treatment_rank)

          # record stands scheduled to be treated in year yr (if dynamic)
          stands_treated <- stands_selected %>%
            left_join(schedule_w_select, by='PA_ID') %>%
            left_join(f_df %>% filter(SCN_ID == !!fire_scenario) %>% dplyr::select(CELL_ID, FIRE_YR, SCN_ID, FIRE_NUMBER), by='CELL_ID') %>%
            dplyr::select(SCN_ID, CELL_ID, PA_ID, ETrt_YR, FIRE_YR, FIRE_NUMBER, treatment_rank, weightedPriority) %>%
            filter(ETrt_YR == 1) %>%
            mutate(ETrt_YR = !!yr) %>%
            bind_rows(stands_treated)

          # remove available stands that were either treated
          stands_available <- stands_available %>%
            filter((CELL_ID %in% stands_treated$CELL_ID == FALSE) & (PA_ID %in% stands_treated$PA_ID == FALSE))

          print(paste(nrow(stands_treated %>% filter(ETrt_YR == yr)), 'stands treated during year', yr))

          if(dynamic_forsys == TRUE) { # remove burnt stands from future selection only if running dynamic forsys

            # record stands that burned this year
            stands_burned <- stands %>%
              left_join(schedule_w_select, by='PA_ID') %>%
              left_join(stands_selected %>% dplyr::select(CELL_ID, weightedPriority)) %>%
              left_join(f_df %>% filter(SCN_ID == !!fire_scenario) %>% dplyr::select(CELL_ID, FIRE_YR, SCN_ID, FIRE_NUMBER), by='CELL_ID') %>%
              filter(FIRE_YR == !!yr) %>%
              dplyr::select(SCN_ID, CELL_ID, PA_ID, ETrt_YR, FIRE_YR, FIRE_NUMBER, treatment_rank, weightedPriority) %>%
              bind_rows(stands_burned)

            # remove available stands that were either treated or burnt
            stands_available <- stands_available %>%
              filter(CELL_ID %in% stands_burned$CELL_ID == FALSE)

            print(paste(nrow(stands_burned %>% filter(FIRE_YR == yr)), 'burnt stands removed from availablity', yr))
          }

      } # END YEAR LOOP

      # !!!!!!!!!!!!!!!!!!!!!!!!!!!
      # !!!!! 4. WRITE DATA !!!!!!!
      # !!!!!!!!!!!!!!!!!!!!!!!!!!!

      # rebuild output stands for output based on stands that were selected dynamically
      stands_selected_out <- stands_prioritized %>% dplyr::select(-weightedPriority) %>%
        inner_join(stands_treated %>% dplyr::select(CELL_ID, ETrt_YR, FIRE_YR, FIRE_NUMBER, weightedPriority), by='CELL_ID') %>%
        arrange(ETrt_YR, -weightedPriority)

      # group selected stands by project
      projects_selected_out <- stands_selected_out %>%
        create_grouped_dataset(grouping_vars = c(stand_group_by, 'ETrt_YR'),
                               summing_vars = c(output_fields, "weightedPriority")) %>%
        arrange(ETrt_YR, -weightedPriority) %>%
        rename_with(.fn = ~ paste0("ETrt_", .x), .cols = output_fields) %>%
        replace(is.na(.), 0)


      # WRITE: write stands to file ---------------

      write_tags <- paste0('_',ifelse(dynamic_forsys, 'Dyn', ''), ifelse(random_projects, 'Ran', ''), ifelse(!dynamic_forsys & !random_projects, 'Std',''))

      print("Producing output files for stands and planning areas")
      stand_fields_to_write = c("SCN_ID", stand_field, stand_group_by, 'ETrt_YR',
                                  'FIRE_YR', 'FIRE_NUMBER', 'AREA_HA', 'aTR_MS')
      stands_selected_out %>%
        mutate(SCN_ID = !!fire_scenario) %>%
        write_stand_outputs_to_file(dir = relative_output_path,
                                  name = paste0(scenario_name, write_tags, '_FScn', fire_scenario),
                                  write_fields = stand_fields_to_write)

      # WRITE: write project to file ---------------

      # group all candidate stands by project
      projects_selected_out <- stands %>%
        compile_planning_areas_and_stands(unique_weights = uniqueWeights,
                                          group_by = stand_group_by,
                                          output_fields = output_fields) %>%
        inner_join(projects_selected_out, by='PA_ID') %>%
        replace(is.na(.), 0) %>%
        mutate(SCN_ID = !!fire_scenario) %>%
        dplyr::select(-ETrt_YR, ETrt_YR) %>%
        arrange(ETrt_YR, -weightedPriority) %>%
        mutate(treatment_rank = ifelse(weightedPriority > 0, 1:n(), NA))

      # assign weight scenario values to project out
      projects_selected_out[,paste0('Pr_', 1:length(priorities), '_', priorities)] = weights[1,]

      # write tag for selection scenario
      project_fn = paste0(relative_output_path, "/proj_", scenario_name, write_tags, "_FScn", fire_scenario, ".csv")
      fwrite(projects_selected_out, file = project_fn, sep = ",", row.names = FALSE)

      } # END WEIGHT LOOP
  }

