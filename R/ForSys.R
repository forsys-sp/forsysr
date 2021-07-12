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
  #' @param proj_target TODO
  #' @param proj_unit TODO
  #' @param proj_target_multiplier TODO
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
    stand_field = 'CELL_ID',
    pcp_spm = c(),
    land_base = '',
    priorities = c(),
    stand_group_by = '',
    proj_unit = '',
    proj_target = '',
    proj_target_multiplier = 0.15,
    proj_fixed_target = FALSE,
    proj_fixed_area_target = NULL,
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
    overwrite_output = TRUE,
    run_with_shiny = FALSE,
    fire_intersect_table = NULL,
    fire_planning_years = 1,
    fire_annual_target_field = NULL,
    fire_annual_target = NA,
    fire_dynamic_forsys = FALSE,
    fire_random_projects = FALSE,
    write_tags = ''
    ) {

    set.seed(1)

    # If a config file has been selected, source it to read in variables
    if (length(config_file) > 0) {
      configuration_file <- config_file
      setwd(dirname(configuration_file))
      source(configuration_file, local = TRUE)
    }

    source('R/forsys_libraries.R')
    source('R/forsys_functions.R')

    # collapse write tags into string if provided as data.frame
    if(write_tags %>% length > 1 & !names(write_tags) %>% is.null){
      write_tags_txt <- paste(names(write_tags), write_tags, sep='_', collapse = '_')
    } else write_tags_txt <- write_tags

    options(scipen = 9999)
    relative_output_path = glue('output/{scenario_name}/{write_tags_txt}')

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
      add_target_field(proj_unit = proj_unit,
                       proj_target = proj_target,
                       proj_target_multiplier = proj_target_multiplier,
                       stand_group_by = stand_group_by,
                       land_base = land_base)

    # create objects for tracking treated and burnt stands
    stands_treated <- NULL
    stands_burned <- NULL

    # specify thresholds
    threshold_dat <- make_thresholds(thresholds = thresholds)

    # set up weighting scenarios
    weights <- weight_priorities(numPriorities = length(priorities),
                                 weights = weighting_values[1])

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

      # threshold = filter
      # constraint = target

      for(yr in 1:fire_planning_years){ # BEGIN YEAR LOOP

        message(paste('---------------\nYear', yr, '\n---------------'))

        # select stands from project areas until target reached while filtering by threshold
        stands_selected <- stands_available %>%
          apply_treatment(
            treatment_type = threshold_dat$type,
            treatment_threshold = threshold_dat$threshold,
            stand_field = stand_field,
            stand_group_by = stand_group_by,
            proj_fixed_target = proj_fixed_target,
            proj_fixed_area_target = proj_fixed_area_target,
            proj_unit = proj_unit,
            proj_target = proj_target,
            proj_target_multiplier = proj_target_multiplier
          )

        # !!!!!!!!!!!!!!!!!!!!!!!!!!!
        # 3. RANK PROJECTS ----------
        # !!!!!!!!!!!!!!!!!!!!!!!!!!!

        # group selected stands by project
        projects_selected <- stands_selected %>%
          create_grouped_dataset(grouping_vars = stand_group_by,
                                 summing_vars = c(output_fields, "weightedPriority")) %>%
          rename_with(.fn = ~ paste0("ETrt_", .x), .cols = output_fields) %>%
          replace(is.na(.), 0) %>%
          arrange(-weightedPriority) %>%
          mutate(treatment_rank = ifelse(weightedPriority > 0, 1:n(), NA)) %>%
          drop_na(treatment_rank)

        # randomize project rank if desired
        if(fire_random_projects){
          message('!! Randomizing projects')

          shuffled_weights <- projects_selected %>%
            filter(treatment_rank %>% is.na == FALSE) %>%
            mutate(weightedPriority = sample(weightedPriority)) %>%
            dplyr::select(stand_group_by, weightedPriority)

           projects_selected <- projects_selected %>% dplyr::select(-weightedPriority) %>%
             left_join(shuffled_weights, by = stand_group_by) %>%
             arrange(-weightedPriority) %>%
             mutate(treatment_rank = ifelse(weightedPriority > 0, 1:n(), NA)) %>%
             drop_na(treatment_rank)
        }

        # !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
        # 4. UPDATE AVAILABILITY ----------
        # !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

        # set annual target
        fire_annual_target_i = fire_annual_target[yr]
        if(is.na(fire_annual_target_i)) fire_annual_target_i = Inf # if no target available, set to Inf

        # schedule projects from year yr into the future based on annual constraint
        if(fire_annual_target_field %>% is.null){
          projects_scheduled <- projects_selected %>%
            mutate(ETrt_YR = 1) %>%
            dplyr::select(stand_group_by, weightedPriority, ETrt_YR, treatment_rank)
        } else {
          projects_scheduled <- projects_selected %>%
            mutate(ETrt_YR = ifelse(cumsum(get(fire_annual_target_field)) %/% !!fire_annual_target_i + 1)) %>%
            mutate(ETrt_YR = ifelse(weightedPriority == 0, NA, ETrt_YR)) %>%
            dplyr::select(stand_group_by, ETrt_YR, treatment_rank) %>%
            filter(ETrt_YR == 1) %>%
            mutate(ETrt_YR = !!yr)
        }

        # record stands scheduled for treatment in current year
        stands_treated <- stands_selected %>%
          inner_join(projects_scheduled, by=stand_group_by) %>%
          dplyr::select(stand_field, stand_group_by, ETrt_YR, treatment_rank) %>%
          bind_rows(stands_treated)

        # remove stands or project areas that were treated from available stands
        stands_available <- stands_available %>%
          filter((.data[[stand_field]] %in% stands_treated[[stand_field]] == FALSE) & (.data[[stand_group_by]] %in% stands_treated[[stand_group_by]] == FALSE))

        # report yearly work
        s_n = stands_treated %>% filter(ETrt_YR == yr) %>% pull(CELL_ID) %>% n_distinct()
        p_n = stands_treated %>% filter(ETrt_YR == yr) %>% pull(PA_ID) %>% n_distinct()
        message(paste0(s_n, ' stands (', round(s_n/nrow(stands_prioritized) * 100, 2), '%) treated in ', p_n, ' projects'))

        if(!is.null(fire_intersect_table)) {
          # record stands that burned this year
          stands_burned <- stands %>%
            left_join(projects_scheduled, by=stand_group_by) %>%
            left_join(stands_selected %>% dplyr::select(stand_field), by=stand_field) %>%
            left_join(fires %>% dplyr::select(stand_field, FIRE_YR, FIRE_NUMBER), by=stand_field) %>%
            filter(FIRE_YR == !!yr) %>%
            dplyr::select(stand_field, stand_group_by, ETrt_YR, FIRE_YR, FIRE_NUMBER, treatment_rank, weightedPriority) %>%
            bind_rows(stands_burned)

          # report yearly fire
          message(paste(stands_burned %>% filter(FIRE_YR == yr) %>% pull(stand_field) %>% n_distinct(), 'stands burned'))

          # remove burnt stands from future selection only if running dynamic forsys
          if(fire_dynamic_forsys == TRUE) {
            stands_available <- stands_available %>% filter(.data[[stand_field]] %in% stands_burned[[stand_field]] == FALSE)
          }

          # order planning areas for treatment post-fire to identify decision-shifts.

        }
      } # END YEAR LOOP

      # !!!!!!!!!!!!!!!!!!!!!!!!!!!
      # 5. WRITE DATA -------------
      # !!!!!!!!!!!!!!!!!!!!!!!!!!!

      # WRITE: write stands to file ...........
      message('Writing output files for stands and planning areas')

      # tag stands with specific scenario attributes
      stands_selected_out <- stands_treated %>% dplyr::select(!!stand_field, !!stand_group_by, ETrt_YR)

      if(!is.null(fire_intersect_table))
        stands_selected_out <- stands_selected_out %>%
          left_join(fires %>% dplyr::select(stand_field, FIRE_YR, FIRE_NUMBER), by=stand_field)

      # write out minimal stand information
      stands_selected_out <- stands_selected_out %>% bind_cols(write_tags)
      stands_selected_out <- stands_selected_out %>%
        left_join(stands_prioritized %>% dplyr::select(!!stand_field, output_fields), by = stand_field)

      stand_fn <- paste0(relative_output_path, "/stnd_", scenario_name, '_', write_tags_txt, ".csv")
      fwrite(stands_selected_out, stand_fn, row.names = FALSE)

      # WRITE: write project to file ...........

      # group *selected* stands by project
      projects_etrt_out <- stands_selected_out %>%
        dplyr::select(!!stand_field, ETrt_YR) %>%
        left_join(stands_prioritized %>% dplyr::select(stand_field, stand_group_by, output_fields, 'weightedPriority'),
                  by = stand_field) %>%
        create_grouped_dataset(grouping_vars = c(stand_group_by, 'ETrt_YR'),
                               summing_vars = c(output_fields, 'weightedPriority')) %>%
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
        arrange(ETrt_YR, -weightedPriority) %>%
        mutate(treatment_rank = ifelse(weightedPriority > 0, 1:n(), NA))

      # tag project with specific scenario attributes
      projects_selected_out <- projects_selected_out %>% bind_cols(write_tags)

      # assign weight scenario values to project out
      projects_selected_out[,paste0('Pr_', 1:length(priorities), '_', priorities)] = weights[1,]

      # write tag for selection scenario
      project_fn = paste0(relative_output_path, "/proj_", scenario_name,  '_', write_tags_txt,".csv")
      fwrite(projects_selected_out, file = project_fn, sep = ",", row.names = FALSE)

      } # END WEIGHT LOOP
  }

