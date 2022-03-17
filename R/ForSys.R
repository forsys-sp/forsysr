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
#' @param shiny_data If data has already been loaded, pass the object here
#' @param num_reps TODO
#' @param config_file Relative path to a config file that defines needed
#' parameters
#' @param scenario_name A name for this scenario
#' @param scenario_stand_filename Path to the input dataset
#' @param stand_id_field The field in the scenario_stand_filename which is a
#' unique ID for each stand
#' @param stand_pcp_spm PCP and SPM values will be calculated for these
#' variables. This should include the priorities and any value outputs.
#' @param global_threshold The land base is the area that is used to calculate
#' the PCP and SPM values. It is currently a single, binary variable that must
#' be computed prior to running the ForSysR script. A blank field means all
#' lands are included in the calculation.
#' @param scenario_priorities Priorities are named here. If only one priority
#' exists, only a weight of one will be used.
#' @param proj_id The field in the scenario_stand_filename that indicates which
#' project or planning area a stand belongs to
#' @param stand_threshold TODO
#' @param proj_fixed_target TODO
#' @param proj_target_field TODO
#' @param proj_target_value TODO
#' @param scenario_weighting_values Defines the weights and integer steps
#' between weights. The values are for min, max, and step.
#' @param scenario_output_fields This should include the desired fields for the
#' planning area treatment files. Planning area id, priority weights and
#' treatment rank are added automatically.
#' @param scenario_output_grouping_fields Include the smaller and larger groups
#' here for grouping of treated stands.
#' @param overwrite_output Overwrite any existing output of the same name?
#' @param run_with_shiny Sets some output business for better shiny interaction
#' @param fire_intersect_table TOTO
#' @param fire_planning_years = TODO
#' @param fire_annual_target_field TODO
#' @param fire_annual_target TODO
#' @param fire_dynamic_forsys logical. Prevent burnt stands from being selected
#' if TRUE
#' @param fire_random_projects logical. Randomly shuffle project prioritization
#' if TRUE
#' @param scenario_write_tags TODO
#'
#' @return Forsys results with weightedPriority, treatmentRank, and weights
#'
#' @importFrom rlang .data
#' @importFrom dplyr %>%
#'
#' @export
run <- function(
    shiny_data = NULL,
    num_reps = 1,
    config_file = NULL,
    scenario_name = "",
    scenario_stand_filename = "",
    stand_id_field = "",
    stand_pcp_spm = NULL,
    global_threshold = NULL,
    scenario_priorities = NULL,
    proj_id = "",
    stand_threshold = NULL,
    proj_fixed_target = FALSE,
    proj_target_field = "",
    proj_target_value = NULL,
    scenario_weighting_values = NULL,
    scenario_output_fields = NULL,
    scenario_output_grouping_fields = NULL,
    overwrite_output = TRUE,
    run_with_shiny = FALSE,
    fire_intersect_table = NULL,
    fire_planning_years = 1,
    fire_annual_target_field = NULL,
    fire_annual_target = NA,
    fire_dynamic_forsys = FALSE,
    fire_random_projects = FALSE,
    scenario_write_tags = NULL

    ) {

    # If a config file has been selected, source it to read in variables
    if (length(config_file) > 0) {
      # setwd(dirname(config_file))
      if (stringr::str_detect(config_file, '[.]R$'))
        source(config_file, local = TRUE)
      if(stringr::str_detect(config_file, '[.]json$'))
        load_json_config(config_file)
    }

    # collapse write tags into string if provided as data.frame
    if(scenario_write_tags %>% length > 1 & !names(scenario_write_tags) %>% is.null){
      scenario_write_tags_txt <- paste(names(scenario_write_tags), scenario_write_tags, sep='_', collapse = '_')
    } else scenario_write_tags_txt <- scenario_write_tags

    options(scipen = 9999)
    relative_output_path = paste0('output/', scenario_name, '/', scenario_write_tags_txt)

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
        message(paste0("output directory, ", absolute_output_path, ", already exists"))
        # if (overwrite_output) list.files(absolute_output_path, full.names = T) %>% file.remove()
        list.files(absolute_output_path, full.names = T) %>% file.remove()
      } else {
        message(paste0("output directory, ", absolute_output_path, ", already exists"))
      }
    }

    # !!!!!!!!!!!!!!!!!!!!!!!!!!!
    # 1. PREP STANDS ------------
    # !!!!!!!!!!!!!!!!!!!!!!!!!!!

    # # # Load data
    if (!is.null(shiny_data)) {
      message("Forsys Shiny data detected.")
      stands <- shiny_data
    } else {
      message("No Forsys Shiny data detected.")
      stands <- load_dataset(scenario_stand_filename)
    }

    if(!is.null(fire_intersect_table)) fires <- fire_intersect_table

    # filter stands by availability
    stands <- stands %>%
      filter_stands(filter_txt = global_threshold)

    # Calculate SPM & PCP values
    stands <- stands %>%
      calculate_spm_pcp(fields = stand_pcp_spm) %>%
      calculate_spm_pcp(fields = scenario_output_fields)

    # set up weighting scenarios
    weights <- weight_priorities(numPriorities = length(scenario_priorities),
                                 weights = scenario_weighting_values[1])

    # Run selection code for each set of weights
    for (w in 1:nrow(weights)) { # START WEIGHT LOOP

      ## Step 0: create the weighted priorities.
      message(paste0("\n---------------\nWeighting scenario ",  w,
                     " of ", nrow(weights),
                     ": ", paste0(weights[w,], collapse = '-')))

      # create objects for tracking treated and burnt stands
      stands_treated <- NULL
      stands_burned <- NULL

      # prep stand data
      stands_prioritized <- stands %>%
        set_up_treatment_types() %>%
        set_up_priorities(
          w = w,
          priorities = paste0(scenario_priorities, '_SPM'),
          weights = weights)

      stands_available <- stands_prioritized

      # !!!!!!!!!!!!!!!!!!!!!!!!!!!
      # 2. SELECT STANDS ------------
      # !!!!!!!!!!!!!!!!!!!!!!!!!!!

      # Reference note: threshold = filter; constraint = target

      for (yr in 1:fire_planning_years) { # BEGIN YEAR LOOP

        if (fire_planning_years > 1) message(paste('\nYear', yr, '\n---------------'))

        # specify thresholds
        threshold_dat <- make_thresholds(stand_threshold)
        # (threshold_dat <- make_thresholds('trt 1: threshold1 == 1; trt 2: priority3 > 0.5'))
        # (threshold_dat <- make_thresholds('trt 1: priority3 > 0.5'))

        stands_selected <- stands_available

        # for(t in 1:length(threshold_dat$type)){

        # set treatment target
        stands_selected <- stands_selected %>%
          set_treatment_target(
            proj_id = proj_id,
            proj_fixed_target = proj_fixed_target,
            proj_target_field = proj_target_field,
            proj_target_value = proj_target_value) %>%
          # apply treatment thresholds
          filter_stands(
            filter_txt = threshold_dat$threshold,
            verbose = T) %>%
          # select stands
          apply_treatment(
            stand_id_field = stand_id_field,
            proj_id = proj_id,
            proj_objective = 'weightedPriority',
            proj_target_field = proj_target_field,
            proj_target = 'master_target'
          ) %>%
          # record treatment name
          dplyr::mutate(treatment_type = !!threshold_dat$type)

        # }

        # !!!!!!!!!!!!!!!!!!!!!!!!!!!
        # 3. RANK PROJECTS ----------
        # !!!!!!!!!!!!!!!!!!!!!!!!!!!

        # group selected stands by project
        projects_selected <- stands_selected %>%
          create_grouped_dataset(
            grouping_vars = proj_id,
            summing_vars = c(scenario_output_fields, "weightedPriority")) %>%
          dplyr::rename_with(.fn = ~ paste0("ETrt_", .x), .cols = scenario_output_fields) %>%
          base::replace(is.na(.), 0) %>%
          dplyr::arrange(-weightedPriority) %>%
          dplyr::mutate(treatment_rank = ifelse(weightedPriority > 0, 1:dplyr::n(), NA)) %>%
          tidyr::drop_na(treatment_rank)

        # FIRE: randomize project rank if desired
        if(fire_random_projects){
          message('!! Randomizing projects')

          shuffled_weights <- projects_selected %>%
            dplyr::filter(treatment_rank %>% is.na == FALSE) %>%
            dplyr::mutate(weightedPriority = sample(weightedPriority)) %>%
            dplyr::select(proj_id, weightedPriority)

           projects_selected <- projects_selected %>% dplyr::select(-weightedPriority) %>%
             dplyr::left_join(shuffled_weights, by = proj_id) %>%
             dplyr::arrange(-weightedPriority) %>%
             dplyr::mutate(treatment_rank = ifelse(weightedPriority > 0, 1:dplyr::n(), NA)) %>%
             tidyr::drop_na(treatment_rank)
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
            dplyr::mutate(ETrt_YR = 1) %>%
            dplyr::select(proj_id, weightedPriority, ETrt_YR, treatment_rank)
        } else {
          projects_scheduled <- projects_selected %>%
            dplyr::mutate(ETrt_YR = cumsum(get(fire_annual_target_field)) %/% !!fire_annual_target_i + 1) %>%
            dplyr::mutate(ETrt_YR = ifelse(weightedPriority == 0, NA, ETrt_YR)) %>%
            dplyr::select(proj_id, ETrt_YR, weightedPriority, treatment_rank) %>%
            dplyr::filter(ETrt_YR == 1) %>%
            dplyr::mutate(ETrt_YR = !!yr)
        }

        # record stands scheduled for treatment in current year
        stands_treated <- stands_selected %>%
          dplyr::inner_join(projects_scheduled, by=proj_id) %>%
          dplyr::select(stand_id_field, proj_id, ETrt_YR, treatment_rank) %>%
          dplyr::bind_rows(stands_treated)

        # remove stands or project areas that were treated from available stands
        stands_available <- stands_available %>%
          dplyr::filter((.data[[stand_id_field]] %in% stands_treated[[stand_id_field]] == FALSE) &
                          (.data[[proj_id]] %in% stands_treated[[proj_id]] == FALSE))

        # report yearly work
        s_n = stands_treated %>% dplyr::filter(ETrt_YR == yr) %>% dplyr::pull(stand_id_field) %>% dplyr::n_distinct()
        p_n = stands_treated %>% dplyr::filter(ETrt_YR == yr) %>% dplyr::pull(proj_id) %>% dplyr::n_distinct()
        message(paste0(s_n, ' stands (', round(s_n/nrow(stands_prioritized) * 100, 2), '%) treated in ', p_n, ' projects'))

        if(!is.null(fire_intersect_table)) { # BEGIN ANNUAL FIRES

          # record stands that burned this year
          stands_burned <- stands %>%
            dplyr::left_join(projects_scheduled, by=proj_id) %>%
            dplyr::left_join(stands_treated %>% dplyr::select(stand_id_field), by=stand_id_field) %>%
            dplyr::left_join(fires %>% dplyr::select(stand_id_field, FIRE_YR, FIRE_NUMBER), by=stand_id_field) %>%
            dplyr::filter(FIRE_YR == !!yr) %>%
            dplyr::select(stand_id_field, proj_id, ETrt_YR, FIRE_YR, FIRE_NUMBER, treatment_rank, weightedPriority) %>%
            dplyr::bind_rows(stands_burned)

          # report yearly fire
          b_n = stands_burned %>% dplyr::filter(FIRE_YR == yr) %>% dplyr::pull(stand_id_field) %>% dplyr::n_distinct()
          message(paste0(b_n, ' (', round(b_n/nrow(stands_prioritized) * 100, 2), '%) stands burned'))

          # remove burnt stands from future selection only if running dynamic forsys
          if(fire_dynamic_forsys == TRUE) {
            stands_available <- stands_available %>%
              dplyr::filter(.data[[stand_id_field]] %in% stands_burned[[stand_id_field]] == FALSE)
          }

        } # END ANNUAL FIRES
      } # END YEAR LOOP

      # !!!!!!!!!!!!!!!!!!!!!!!!!!!
      # 5. WRITE DATA -------------
      # !!!!!!!!!!!!!!!!!!!!!!!!!!!

      # tag stands with specific scenario attributes
      stands_selected_out <- stands_treated %>%
                              dplyr::select(!!stand_id_field, !!proj_id, ETrt_YR)

      if(!is.null(fire_intersect_table))
        stands_selected_out <- stands_selected_out %>%
          dplyr::left_join(fires %>% dplyr::select(stand_id_field, FIRE_YR, FIRE_NUMBER), by=stand_id_field)


      # update project output fields to include all PCP fields if present
      pcp_fields <- dplyr::select(stands, matches('_PCP$')) %>% names()
      scenario_output_fields <- c(scenario_output_fields, pcp_fields)

      # ........................................
      # write stands to file ...................
      # ........................................

      stands_selected_out <- stands_selected_out %>%
        dplyr::bind_cols(scenario_write_tags)

      stands_selected_out <- stands_selected_out %>%
        dplyr::left_join(stands_prioritized %>%
                           dplyr::select(!!stand_id_field, scenario_output_fields),
                         by = stand_id_field)

      # assign weight scenario values to stand out out
      stands_selected_out[,paste0('Pr_', 1:length(scenario_priorities), '_', scenario_priorities)] = weights[w,]

      if (length(scenario_write_tags_txt) > 1) {
        stand_fn <- paste0(relative_output_path, "/stnd_", scenario_name, '_', scenario_write_tags_txt, ".csv")
      } else {
        stand_fn <- paste0(relative_output_path, "/stnd_", scenario_name, ".csv")
      }

      data.table::fwrite(stands_selected_out, stand_fn, row.names = FALSE, append = TRUE)

      # ........................................
      # write project to file ..................
      # ........................................

      # summarize selected stands by grouping fields (eg project)
      projects_etrt_out <- stands_selected_out %>%
        dplyr::select(!!stand_id_field, ETrt_YR) %>%
        dplyr::left_join(stands_prioritized %>%
                           dplyr::select(
                             stand_id_field,
                             proj_id,
                             scenario_output_grouping_fields,
                             scenario_output_fields,
                             'weightedPriority'),
                         by = stand_id_field) %>%
        create_grouped_dataset(
          grouping_vars = c(scenario_output_grouping_fields, 'ETrt_YR'),
          summing_vars = c(scenario_output_fields, 'weightedPriority')
          ) %>%
        dplyr::arrange(ETrt_YR, -weightedPriority) %>%
        dplyr::rename_with(.fn = ~ paste0("ETrt_", .x), .cols = scenario_output_fields) %>%
        base::replace(is.na(.), 0)

      # rank projects
      projects_rank <- projects_etrt_out %>%
        dplyr::group_by(!!proj_id := get(proj_id)) %>%
        dplyr::summarize_at('weightedPriority', sum) %>%
        dplyr::arrange(-weightedPriority) %>%
        dplyr::mutate(treatment_rank = rank(-weightedPriority)) %>%
        dplyr::select(!!proj_id, treatment_rank)

      # summarize available stands by grouping fields (eg project)
      projects_esum_out <- stands %>%
        compile_planning_areas_and_stands(
          unique_weights = uniqueWeights,
          group_by = scenario_output_grouping_fields,
          output_fields = scenario_output_fields)

      # combine etrt w/ esum w/ project treatment rank
      projects_selected_out <- projects_etrt_out %>%
        dplyr::inner_join(projects_esum_out, by=scenario_output_grouping_fields) %>%
        dplyr::left_join(projects_rank, by = proj_id) %>%
        base::replace(is.na(.), 0) %>%
        dplyr::arrange(treatment_rank)

      # tag project with specific scenario attributes
      projects_selected_out <- projects_selected_out %>%
        dplyr::bind_cols(scenario_write_tags)

      # assign weight scenario values to project out
      projects_selected_out[,paste0('Pr_', 1:length(scenario_priorities), '_', scenario_priorities)] = weights[w,]

      # write tag for selection scenario
      if (length(scenario_write_tags_txt) > 1) {
        project_fn = paste0(relative_output_path, "/proj_", scenario_name,  '_', scenario_write_tags_txt, ".csv")
      } else {
        project_fn = paste0(relative_output_path, "/proj_", scenario_name, ".csv")
      }

      data.table::fwrite(projects_selected_out, file = project_fn, sep = ",", row.names = FALSE, append = TRUE)

      } # END WEIGHT LOOP

    message('Forsys simulation is complete')
    return(projects_selected_out)
  }

