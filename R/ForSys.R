#' ForSysR
#'
#' Primary function for running the ForSys treatment planner. Either provide parameters, or define parameters
#' in a config file and pass the name of the file to this run function.
#'
#' @param stand_data If data has already been loaded, pass the object here
#' @param config_file Relative path to a config file that defines needed parameters
#' @param scenario_name A name for this scenario
#' @param scenario_stand_filename Path to the input dataset
#' @param stand_id_field The field in the scenario_stand_filename which is a unique ID for each stand
#' @param stand_area_field string of field containing stand area
#' @param stand_pcp_spm PCP and SPM values will be calculated for these variables. This should include the priorities and any value outputs.
#' @param global_threshold The land base is the area that is used to calculate
        #' the PCP and SPM values. It is currently a single, binary variable that must
        #' be computed prior to running the ForSysR script. A blank field means all
        #' lands are included in the calculation.
#' @param scenario_priorities Priorities are named here. If only one priority exists, only a weight of one will be used.
#' @param proj_id_field string of field in the scenario_stand_filename that indicates which project or planning area a stand belongs to.
#' @param stand_threshold string Boolean statement used as threshold for whether stands are counted as part of a project
#' @param proj_fixed_target logical describing if target is fixed or relative
#' @param proj_target_field string of stand field used as target constraint
#' @param proj_target_value numeric value describing the target constraint either an fixed value if proj_fixed_target is TRUE or a proportion of the project sum for the proj_target_field specified.
#' @param scenario_weighting_values Defines the weights and integer steps between weights. The values are for min, max, and step.
#' @param scenario_output_fields This should include the desired fields for the
        #' planning area treatment files. Planning area id, priority weights and
        #' treatment rank are added automatically.
#' @param scenario_output_grouping_fields Include the smaller and larger groups here for grouping of treated stands.
#' @param overwrite_output logical whether to overwrite any existing output of the same name
#' @param run_with_shiny logical whether run was called from within shiny.
#' @param run_with_fire logical whether to forsys alongside fire
#' @param run_with_patchmax logical whether PatchMax should be used for building projects
#' @param fire_intersect_table data frame listing stands affected by fire by year
#' @param fire_planning_years number of years to run forsys
#' @param fire_annual_target_field TODO
#' @param fire_annual_target TODO
#' @param fire_dynamic_forsys logical. Prevent burnt stands from being selected if TRUE
#' @param fire_random_projects logical. Randomly shuffle project prioritization if TRUE
#' @param scenario_write_tags TODO
#' @param proj_treatment_name TODO
#' @param patchmax_stnd_adj igraph object describe patch adjacency
#' @param patchmax_proj_number TODO
#' @param patchmax_proj_size Integer. Number of patchmax stands to estimate
#' @param patchmax_proj_size_slack TODO
#' @param patchmax_candidate_min_size TODO
#' @param patchmax_sample_n TODO
#' @param patchmax_sample_seed TODO
#'
#' @return Forsys results with weightedPriority, treatmentRank, and weights
#'
#' @importFrom rlang .data
#' @importFrom dplyr %>%
#'
#' @export
run <- function(
    config_file = NULL,
    scenario_name = "",
    scenario_stand_filename = "",
    stand_data = NULL,
    stand_id_field = "",
    proj_id_field = "",
    stand_pcp_spm = NULL,
    stand_area_field = NULL,
    scenario_priorities = NULL,
    global_threshold = NULL,
    stand_threshold = NULL,
    proj_treatment_name = "",
    proj_fixed_target = FALSE,
    proj_target_field = "",
    proj_target_value = NULL,
    scenario_weighting_values = NULL,
    scenario_output_fields = NULL,
    scenario_output_grouping_fields = NULL,
    # module toggles
    run_with_shiny = FALSE,
    run_with_patchmax = FALSE,
    run_with_fire = FALSE,
    # fire arguments
    fire_intersect_table = NULL,
    fire_planning_years = 1,
    fire_annual_target_field = NULL,
    fire_annual_target = NA,
    fire_dynamic_forsys = FALSE,
    fire_random_projects = FALSE,
    # patchmax arguments
    patchmax_stnd_adj = NULL, # required
    patchmax_proj_number = 1, # required
    patchmax_proj_size = Inf, # required
    patchmax_proj_size_slack  = NULL, # required
    patchmax_candidate_min_size = NULL, # optional
    patchmax_sample_n = NULL, # optional
    patchmax_sample_seed = NULL, # optional
    # misc
    scenario_write_tags = NULL,
    overwrite_output = TRUE
    ) {

    # source config file if provided
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
    if (!is.null(stand_data)) {
      message("Forsys Shiny data detected.")
      stands <- stand_data
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

      for (yr in 1:fire_planning_years) { # BEGIN YEAR LOOP

        if (fire_planning_years > 1) message(paste('\nYear', yr, '\n---------------'))

        if(run_with_patchmax){

          require(Patchmax)

          # Switch for using PatchMax to build projects. Workflow: 1) build projects
          # until annual target is reached (TODO: add while statement). 2)
          # compile project statistics based on selected stands (e.g., sum
          # weightingValue and scenario output fields), 3) repeat in subsequent
          # year.

          # TODO shiny does not passing stand_area_field
          stand_area_field = 'area_ha'

          # TODO needs ot fail if not igraph adjacency is not provided
          load('~/GitHub/forsys-data/test_adj.Rdata')
          if(is.null(patchmax_stnd_adj)){
            message('No adjacency objective provided; building from scratch (this can take several minutes)')
            shp <- st_read('data/STF_forsystest/STF_hexnet_test.shp') %>% st_make_valid()
            patchmax_stnd_adj <- Patchmax::calculate_adj(shp, St_id = shp$CELL_ID)
          }

          # helper for translating stand threshold into fields
          threshold_dat <- Patchmax::forsys_helper_prep_threshold(stand_threshold)

          # create list of arguments for running patchmax
          patchmax_args <- list(
            adj_object = adj_object,
            st_id = stands_available %>% pull(!!stand_id_field), # stand id vector
            st_area = stands_available %>% pull(!!stand_area_field), # stand area vector
            st_objective = stands_available %>% pull(weightedPriority), # vector of stand values to maximize
            p_size = 25000, # numeric project size constraint based on St_area
            p_size_slack = NULL,  # 0-1 numeric setting flexibility in hitting P_size constraint
            p_number = 2, # integer count of projects to create
            sample_n = 1000, # integer number of candidate stands used to build projects
            sample_seed = NULL, # set seed for random seed selection
            st_threshold = stands_available %>% pull(!!threshold_dat$st_threshold),
            st_threshold_value = threshold_dat$st_threshold_value
          )

          # run patchmax
          patchmax_out <- Patchmax::simulate_projects(
            St_id = patchmax_args$st_id,
            St_area = patchmax_args$st_area,
            St_objective = patchmax_args$st_objective,
            St_adj = patchmax_args$adj_object, # igraph object
            P_size = patchmax_args$p_size,
            P_size_slack  = patchmax_args$p_size_slack,
            P_number = patchmax_args$p_number,
            St_threshold = patchmax_args$st_threshold,
            St_threshold_value = patchmax_args$st_threshold_value,
            P_constraint = NULL,
            P_constraint_min_value = NULL,
            P_constraint_max_value = NULL,
            Candidate_min_size = NULL,
            Sample_n = patchmax_args$sample_n,
            Sample_seed = patchmax_args$sample_seed
          )

          # extract stands selected in PatchMax
          patchmax_stands <- patchmax_out[[2]] %>%
            dplyr::select(!!stand_id_field := Stands,
                          patchmax_proj_id := Project)

          # identify stands selected from stands available
          stands_selected <- stands_available %>%
            inner_join(patchmax_stands) %>%
            dplyr::select(-!!proj_id_field) %>%
            dplyr::mutate(!!proj_id_field := patchmax_proj_id) %>%
            dplyr::mutate(treatment_type = !!proj_treatment_name)

        } else {

          # if run_with_patchmax == FALsE assume static project areas

          # stand selection based on predetermined projects
          stands_selected <- stands_available %>%
            # set treatment target
            set_treatment_target(
              proj_id_field = proj_id_field,
              proj_fixed_target = proj_fixed_target,
              proj_target_field = proj_target_field,
              proj_target_value = proj_target_value) %>%
            # apply treatment thresholds
            filter_stands(
              filter_txt = stand_threshold,
              verbose = T) %>%
            # select stands
            apply_treatment(
              stand_id_field = stand_id_field,
              proj_id_field = proj_id_field,
              proj_objective = 'weightedPriority',
              proj_target_field = proj_target_field,
              proj_target = 'master_target'
            ) %>%
            # record treatment name
            dplyr::mutate(treatment_type = !!proj_treatment_name)

        }

        # !!!!!!!!!!!!!!!!!!!!!!!!!!!
        # 3. RANK PROJECTS ----------
        # !!!!!!!!!!!!!!!!!!!!!!!!!!!

        # group selected stands by project, summarize, and rank
        projects_selected <- stands_selected %>%
          create_grouped_dataset(
            grouping_vars = proj_id_field,
            summing_vars = c(scenario_output_fields, "weightedPriority")) %>%
          dplyr::rename_with(.fn = ~ paste0("ETrt_", .x), .cols = scenario_output_fields) %>%
          base::replace(is.na(.), 0) %>%
          dplyr::arrange(-weightedPriority) %>%
          dplyr::mutate(treatment_rank = ifelse(weightedPriority > 0, 1:dplyr::n(), NA)) %>%
          tidyr::drop_na(treatment_rank)

        # FIRE: randomize project rank if desired
        # TODO delete fire_random_projects in future given this is very project specific

        if(fire_random_projects){
          message('!! Randomizing projects')

          shuffled_weights <- projects_selected %>%
            dplyr::filter(treatment_rank %>% is.na == FALSE) %>%
            dplyr::mutate(weightedPriority = sample(weightedPriority)) %>%
            dplyr::select(proj_id_field, weightedPriority)

           projects_selected <- projects_selected %>% dplyr::select(-weightedPriority) %>%
             dplyr::left_join(shuffled_weights, by = proj_id_field) %>%
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
        if(is.null(fire_annual_target_field)){
          projects_scheduled <- projects_selected %>%
            dplyr::mutate(ETrt_YR = 1) %>%
            dplyr::select(proj_id_field, weightedPriority, ETrt_YR, treatment_rank)
        } else {
          projects_scheduled <- projects_selected %>%
            dplyr::mutate(ETrt_YR = cumsum(get(fire_annual_target_field)) %/% !!fire_annual_target_i + 1) %>%
            dplyr::mutate(ETrt_YR = ifelse(weightedPriority == 0, NA, ETrt_YR)) %>%
            dplyr::select(proj_id_field, ETrt_YR, weightedPriority, treatment_rank) %>%
            dplyr::filter(ETrt_YR == 1) %>%
            dplyr::mutate(ETrt_YR = !!yr)
        }

        # record stands scheduled for treatment in current year
        stands_treated <- stands_selected %>%
          dplyr::inner_join(projects_scheduled, by=proj_id_field) %>%
          dplyr::select(stand_id_field, proj_id_field, ETrt_YR, treatment_rank) %>%
          dplyr::bind_rows(stands_treated)

        # remove stands or project areas that were treated from available stands
        x1 = unique(stands_treated[[stand_id_field]])
        x2 = unique(stands_treated[[proj_id_field]])
        stands_available <- stands_available %>%
          dplyr::filter(.data[[stand_id_field]] %in% x1 == FALSE) %>%
          dplyr::filter(.data[[proj_id_field]] %in% x2 == FALSE)

        # report yearly work
        s_n = stands_treated %>% dplyr::filter(ETrt_YR == yr) %>% dplyr::pull(stand_id_field) %>% dplyr::n_distinct()
        p_n = stands_treated %>% dplyr::filter(ETrt_YR == yr) %>% dplyr::pull(proj_id_field) %>% dplyr::n_distinct()
        message(paste0(s_n, ' stands (', round(s_n/nrow(stands_prioritized) * 100, 2), '%) treated in ', p_n, ' projects'))

        # BEGIN ANNUAL FIRES
        if(run_with_fire & !is.null(fire_intersect_table)) {

          # record stands that burned this year
          stands_burned <- stands %>%
            dplyr::left_join(projects_scheduled, by=proj_id_field) %>%
            dplyr::left_join(stands_treated %>% dplyr::select(stand_id_field), by=stand_id_field) %>%
            dplyr::left_join(fires %>% dplyr::select(stand_id_field, FIRE_YR, FIRE_NUMBER), by=stand_id_field) %>%
            dplyr::filter(FIRE_YR == !!yr) %>%
            dplyr::select(stand_id_field, proj_id_field, ETrt_YR, FIRE_YR, FIRE_NUMBER, treatment_rank, weightedPriority) %>%
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
                              dplyr::select(!!stand_id_field, !!proj_id_field, ETrt_YR)

      if(run_with_fire & !is.null(fire_intersect_table)){
        stands_selected_out <- stands_selected_out %>%
          dplyr::left_join(fires %>% dplyr::select(stand_id_field, FIRE_YR, FIRE_NUMBER), by=stand_id_field)
      }

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
                             proj_id_field,
                             scenario_output_grouping_fields,
                             scenario_output_fields,
                             'weightedPriority'),
                         by = stand_id_field) %>%
        create_grouped_dataset(
          grouping_vars = c(proj_id_field, scenario_output_grouping_fields, 'ETrt_YR'),
          summing_vars = c(scenario_output_fields, 'weightedPriority')
          ) %>%
        dplyr::arrange(ETrt_YR, -weightedPriority) %>%
        dplyr::rename_with(.fn = ~ paste0("ETrt_", .x), .cols = scenario_output_fields) %>%
        base::replace(is.na(.), 0)

      # summarize available stands by grouping fields (eg project)
      projects_esum_out <- stands %>%
        compile_planning_areas_and_stands(
          unique_weights = uniqueWeights,
          group_by = c(proj_id_field, scenario_output_grouping_fields),
          output_fields = scenario_output_fields)

      # combine etrt w/ esum
      projects_etrt_esum_out <- projects_etrt_out %>%
        dplyr::inner_join(projects_esum_out, by=unique(c(proj_id_field, scenario_output_grouping_fields))) %>%
        base::replace(is.na(.), 0)

      # rank projects
      projects_rank <- projects_etrt_out %>%
        dplyr::group_by(!!proj_id_field := get(proj_id_field)) %>%
        dplyr::summarize_at('weightedPriority', sum) %>%
        dplyr::arrange(-weightedPriority) %>%
        dplyr::mutate(treatment_rank = rank(-weightedPriority)) %>%
        dplyr::select(!!proj_id_field, treatment_rank)

      # tag weighting scenario
      priority_write_tags <- weights[w,] %>%
        setNames(paste0('Pr_', 1:length(scenario_priorities), '_', scenario_priorities))

      # tag subset output with treatment rank, scenario_write_tags, priority weights
      subset_out <- projects_etrt_esum_out %>%
        dplyr::left_join(projects_rank, by = proj_id_field) %>%
        dplyr::arrange(treatment_rank) %>%
        dplyr::bind_cols(scenario_write_tags) %>%
        dplyr::bind_cols(priority_write_tags)

      # tag project output with treatment rank, scenario_write_tags, priority weights
      projects_out <- projects_etrt_esum_out %>%
        # dplyr::select(-scenario_output_grouping_fields) %>%
        dplyr::group_by(!!proj_id_field := get(proj_id_field), ETrt_YR) %>%
        summarize_if(is.numeric, sum) %>%
        dplyr::left_join(projects_rank, by = proj_id_field) %>%
        dplyr::arrange(treatment_rank) %>%
        dplyr::bind_cols(scenario_write_tags) %>%
        dplyr::bind_cols(priority_write_tags)

      # write tag for selection scenario
      if (length(scenario_write_tags_txt) > 1) {
        project_fn = paste0(relative_output_path, "/proj_", scenario_name,  '_', scenario_write_tags_txt, ".csv")
        subset_fn = paste0(relative_output_path, "/subset_", scenario_name,  '_', scenario_write_tags_txt, ".csv")
      } else {
        project_fn = paste0(relative_output_path, "/proj_", scenario_name, ".csv")
        subset_fn = paste0(relative_output_path, "/subset_", scenario_name, ".csv")
      }

      data.table::fwrite(projects_out, file = project_fn, sep = ",", row.names = FALSE, append = TRUE)
      data.table::fwrite(subset_out, file = subset_fn, sep = ",", row.names = FALSE, append = TRUE)

      } # END WEIGHT LOOP

    message('Forsys simulation is complete')
  }
