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
    fire_dynamic_forsys = FALSE,
    fire_random_projects = FALSE,
    fire_intersect_table = NULL,
    write_tags = '',
    ...
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
    relative_output_path = glue('output/{scenario_name}{write_tags}')

    # Check if output directory exists
    absolute_output_path = file.path(getwd(), relative_output_path)
    if (!dir.exists(absolute_output_path)) {
      if (run_with_shiny) {

      } else {
        message(paste0("Making output directory: ", absolute_output_path))
      }
      dir.create(absolute_output_path, recursive=TRUE)
    } else {
      if (run_with_shiny) {

      } else {
        message(paste0("output directory, ", absolute_output_path, ", already exists"))
      }
    }

    # !!!!!!!!!!!!!!!!!!!!!!!!!!!
    # 1. PREP STANDS ------------
    # !!!!!!!!!!!!!!!!!!!!!!!!!!!

    # # # Load data
    stands <- load_dataset(input_standfile)
    if(!is.null(fire_intersect_table)) fires <- fire_intersect_table

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
        message(paste0("Creating weighted priorities:",  w, " of ", nrow(weights)))
      }

      # prep stand data
      stands_prioritized <- stands %>%
        set_up_treatment_types() %>%
        set_up_priorities(w = w,
                          priorities = priorities,
                          weights = weights)

      stands_available <- stands_prioritized

      # !!!!!!!!!!!!!!!!!!!!!!!!!!!
      # 2. SELECT STANDS ------------
      # !!!!!!!!!!!!!!!!!!!!!!!!!!!

      for(yr in 1:planning_years){ # BEGIN YEAR LOOP

        message(paste('---------------\nYear', yr, '\n---------------'))

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
        # 3. RANK PROJECTS ----------
        # !!!!!!!!!!!!!!!!!!!!!!!!!!!

        # group selected stands by project
        projects_selected <- stands_selected %>%
          create_grouped_dataset(grouping_vars = stand_group_by, summing_vars = c(output_fields, "weightedPriority")) %>%
          rename_with(.fn = ~ paste0("ETrt_", .x), .cols = output_fields) %>%
          replace(is.na(.), 0) %>%
          arrange(-weightedPriority) %>%
          mutate(treatment_rank = ifelse(weightedPriority > 0, 1:n(), NA))
        fwrite(projects_selected, "prioritized_projects.csv")

        # randomize project rank if desired
        if(fire_random_projects){
          message('!! Randomizing projects')

          p_weights_shuffled <- projects_selected %>%
            filter(treatment_rank %>% is.na == FALSE) %>%
            mutate(weightedPriority = sample(weightedPriority)) %>%
            dplyr::select(PA_ID, weightedPriority)

           projects_selected <- projects_selected %>% dplyr::select(-weightedPriority) %>%
             left_join(p_weights_shuffled, by = stand_group_by) %>%
             arrange(-weightedPriority) %>%
             mutate(treatment_rank = ifelse(weightedPriority > 0, 1:n(), NA))
        }

        # !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
        # 4. UPDATE AVAILABILITY ----------
        # !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

        # set annual target
        annual_project_target_i = annual_project_target[yr]
        if(is.na(annual_project_target_i)) annual_project_target_i = 0 # if no target available, set to 0

        # schedule projects from year yr into the future based on annual constraint
        projects_scheduled <- projects_selected %>%
          mutate(ETrt_YR = cumsum(ETrt_AREA_HA) %/% !!annual_project_target_i + 1) %>%
          mutate(ETrt_YR = ifelse(weightedPriority == 0, NA, ETrt_YR)) %>%
          dplyr::select(PA_ID, ETrt_YR, treatment_rank) %>%
          filter(ETrt_YR == 1) %>%
          mutate(ETrt_YR = !!yr)

        # record stands scheduled for treatment in current year
        stands_treated <- stands_selected %>%
          inner_join(projects_scheduled, by=stand_group_by) %>%
          dplyr::select(CELL_ID, PA_ID, ETrt_YR, treatment_rank, weightedPriority) %>%
          bind_rows(stands_treated)

        # if(simulate_fires) stands_treated <- stands_treated %>% left_join(fires %>% dplyr::select(CELL_ID, FIRE_YR, FIRE_NUMBER))

        # remove stands or project areas that were treated from available stands
        stands_available <- stands_available %>%
          filter((CELL_ID %in% stands_treated$CELL_ID == FALSE) & (PA_ID %in% stands_treated$PA_ID == FALSE))

        # report yearly work
        message(paste(stands_treated %>% filter(ETrt_YR == yr) %>% pull(CELL_ID) %>% n_distinct(),
                    'stands treated in',
                    stands_treated %>% filter(ETrt_YR == yr) %>% pull(PA_ID) %>% n_distinct(),
                    'projects'))


        if(!is.null(fire_intersect_table)) {
          # stands_treated <- stands_treated %>% left_join(fires %>% dplyr::select(CELL_ID, FIRE_YR, FIRE_NUMBER), by=stand_field)

          # record stands that burned this year
          stands_burned <- stands %>%
            left_join(projects_scheduled, by=stand_group_by) %>%
            left_join(stands_selected %>% dplyr::select(CELL_ID, weightedPriority), by=stand_field) %>%
            left_join(fires %>% dplyr::select(CELL_ID, FIRE_YR, FIRE_NUMBER), by=stand_field) %>%
            filter(FIRE_YR == !!yr) %>%
            dplyr::select(CELL_ID, PA_ID, ETrt_YR, FIRE_YR, FIRE_NUMBER, treatment_rank, weightedPriority) %>%
            bind_rows(stands_burned)

          # report yearly fire
          message(paste(stands_burned %>% filter(FIRE_YR == yr) %>% pull(CELL_ID) %>% n_distinct(), 'stands burned'))

          # remove burnt stands from future selection only if running dynamic forsys
          if(fire_dynamic_forsys == TRUE) {
            stands_available <- stands_available %>% filter(CELL_ID %in% stands_burned$CELL_ID == FALSE)
          }

          # order planning areas for treatment post-fire to identify decision-shifts.

        }
      } # END YEAR LOOP

      # !!!!!!!!!!!!!!!!!!!!!!!!!!!
      # 5. WRITE DATA -------------
      # !!!!!!!!!!!!!!!!!!!!!!!!!!!

      message('Writing output files for stands and planning areas')
      write_tags <- paste0(write_tags, '_', paste(names(tibble(...)), as.numeric(tibble(...)), sep = '', collapse = '_'))

      # WRITE: write stands to file ...........

      # tag stands with specific scenario attributes
      stands_selected_out <- stands_treated %>% dplyr::select(CELL_ID,  ETrt_YR) %>%
        left_join(stands_prioritized %>% dplyr::select(CELL_ID, PA_ID, weightedPriority, AREA_HA, aTR_MS)) %>%
        bind_cols(...)

      stand_fields_to_write = c(stand_field, stand_group_by, 'ETrt_YR', 'AREA_HA', 'aTR_MS', names(tibble(...)))
      if(!is.null(fire_intersect_table)) {
        stand_fields_to_write <- c(stand_fields_to_write, 'FIRE_YR')
        stands_selected_out <- stands_selected_out %>% merge(fires, all.x = TRUE)
      }

      stand_fn <- paste0(relative_output_path, "/stnd_", scenario_name, write_tags, ".csv")
      fwrite(stands_selected_out %>% dplyr::select(stand_fields_to_write), stand_fn, row.names = FALSE)

      # WRITE: write project to file ...........


      # group *selected* stands by project
      projects_etrt_out <- stands_selected_out %>%
        merge(stands_prioritized %>% dplyr::select('CELL_ID', output_fields, 'weightedPriority'), by = c('AREA_HA', 'aTR_MS', 'CELL_ID', 'weightedPriority'), all.x = TRUE) %>%
        create_grouped_dataset(grouping_vars = c(stand_group_by, 'ETrt_YR'), summing_vars = c(output_fields, 'weightedPriority')) %>%
        arrange(ETrt_YR, -weightedPriority) %>%
        rename_with(.fn = ~ paste0("ETrt_", .x), .cols = output_fields) %>%
        replace(is.na(.), 0)

      # group *all* stands by project
      projects_esum_out <- stands %>%
        compile_planning_areas_and_stands(unique_weights = uniqueWeights,
                                          group_by = stand_group_by,
                                          output_fields = output_fields)

      # combine etrt w/ esum
      projects_selected_out <- projects_etrt_out %>%
        inner_join(projects_esum_out, by=stand_group_by) %>%
        replace(is.na(.), 0) %>%
        dplyr::select(-ETrt_YR, ETrt_YR) %>%
        arrange(ETrt_YR, -weightedPriority) %>%
        mutate(treatment_rank = ifelse(weightedPriority > 0, 1:n(), NA))

      # tag project with specific scenario attributes
      projects_selected_out <- projects_selected_out %>% bind_cols(...)

      # assign weight scenario values to project out
      projects_selected_out[,paste0('Pr_', 1:length(priorities), '_', priorities)] = weights[1,]

      # write tag for selection scenario
      project_fn = paste0(relative_output_path, "/proj_", scenario_name, write_tags,".csv")
      fwrite(projects_selected_out, file = project_fn, sep = ",", row.names = FALSE)

      } # END WEIGHT LOOP
  }

