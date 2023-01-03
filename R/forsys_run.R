#' ForSysR
#'
#' Primary function for running the ForSys treatment planner. Either provide 
#' parameters, or define parameters in a config file and pass the name of the 
#' file to this run function.
#'
#' @param config_file relative path to a config file that defines needed parameters
#' @param scenario_name name for this scenario
#' @param stand_data input stand dataset
#' @param stand_data_filename path to the input dataset (if not passed)
#' @param stand_id_field The field in the stand_data_filename which is a unique ID for each stand
#' @param stand_area_field string of field containing stand area
#' @param stand_pcp_spm character vector of fields to calculate PCP and SPM values.
#' This should include the priorities and any value outputs. If null, use scenario priority fields
#' @param global_threshold soolean statement passed as a string used to define 
#' stands within the scenario. Excluded stands are not considered part of the 
#' problem so are not used to calculate PCP or ESum values.
#' @param normalize_values logical whether spm fields should be normalized
#' @param scenario_priorities Priorities are named here. If only one priority 
#' exists, only a weight of one will be used.
#' @param proj_id_field string of field in the stand_data_filename that indicates 
#' which project or planning area a stand belongs to.
#' @param stand_threshold Boolean statement passed as a string and used as 
#' threshold for whether stands are counted towards project objective
#' @param proj_fixed_target logical describing if target is fixed or relative
#' @param proj_target_field string of stand field used as target constraint
#' @param proj_target_value numeric value for target constraint, either an fixed
#' value if proj_fixed_target == TRUE or a value between 0 and 1 
#' if proj_fixed_target is FALSE.
#' @param scenario_weighting_values string of 3 integers separated by a space 
#' that defines the weighting min, max, and step.
#' @param scenario_output_fields vector of field names to write out
#' @param scenario_output_grouping_fields Include the smaller and larger groups 
#' here for grouping of treated stands.
#' @param overwrite_output logical whether to overwrite any existing output of the same name
#' @param run_with_shiny logical whether run was called from within shiny.
#' @param run_with_fire logical whether to forsys alongside fire
#' @param run_with_patchmax logical whether PatchMax should be used for building projects
#' @param fire_intersect_table data frame listing stands affected by fire by year
#' @param fire_intersect_table_filename data frame listing stands affected by fire by year
#' @param fire_planning_years number of years to run forsys
#' @param fire_annual_target_field character field to use for calculating annual target
#' @param fire_annual_target numeric value of annual cumulative target
#' @param fire_dynamic_forsys logical. Prevent burnt stands from being selected if TRUE
#' @param fire_random_projects logical. Randomly shuffle project prioritization if TRUE
#' @param scenario_write_tags optional string appended to output used to describe scenario
#' @param proj_treatment_name optional string appended to output used to name treatment
#' @param patchmax_stnd_adj igraph object describe patch adjacency
#' @param patchmax_stnd_adj_filename igraph object describe patch adjacency
#' @param patchmax_proj_number integer number of projects to build
#' @param patchmax_proj_size integer value of target area for each project
#' @param patchmax_proj_size_slack Numeric between 0 and 1 represent percent of 
#' slack allowed in project size constraint
#' @param patchmax_candidate_min_size TODO
#' @param patchmax_st_seed set stand seed IDs
#' @param patchmax_st_distance Stand distance table. Coupled with SDW parameter. 
#' If NULL, then stand distance weight function is not applied.
#' @param patchmax_SDW Stand distance weight parameter. If NULL, then 
#' the value = 1 is used by default.
#' @param patchmax_sample_n Integer count of stands to randomly sample (useful for speeding up patchmax during testing)
#' @param return_outputs
#'
#' @return Forsys results with weightedPriority, treatmentRank, and weights
#'
#' @importFrom rlang .data
#' @importFrom dplyr %>% select mutate rename arrange filter left_join inner_join bind_rows bind_cols n n_distinct
#'
#' @export
run <- function(
    config_file = NULL,
    return_outputs = FALSE,
    # basic
    stand_data = NULL,
    stand_data_filename = "",
    stand_id_field = "",
    stand_area_field = NULL,
    stand_pcp_spm = NULL, # TODO rename scenario_pcp_spm
    stand_threshold = NULL, # TODO rename proj_stand_threshold
    global_threshold = NULL, # TODO rename scenario_stand_threshold
    normalize_values = TRUE,
    # project variables
    proj_id_field = "proj_id",
    proj_fixed_target = TRUE,
    proj_target_field = NULL,
    proj_target_value = NULL, # TODO rename to proj_target_max_value
    proj_target_min_value = -Inf,
    proj_treatment_name = "",
    # scenario variables
    scenario_name = "",
    scenario_priorities = NULL,
    scenario_weighting_values = "1 1 1", # TODO separate to 3 parameters? vector of length 3?
    scenario_output_fields = NULL,
    scenario_output_grouping_fields = NULL,
    scenario_write_tags = NULL,
    overwrite_output = TRUE,
    # module toggles
    run_with_shiny = FALSE,
    run_with_patchmax = FALSE,
    run_with_fire = FALSE,
    # fire arguments
    fire_intersect_table = NULL,
    fire_intersect_table_filename = NULL,
    fire_planning_years = 1,
    fire_annual_target_field = NULL,
    fire_annual_target = NA,
    fire_dynamic_forsys = FALSE,
    fire_random_projects = FALSE,
    # patchmax arguments # << TODO delete
    # patchmax_stnd_adj = NULL, # << TODO delete
    # patchmax_stnd_adj_filename = NULL, # << TODO delete
    patchmax_proj_number = 1,
    patchmax_proj_size = Inf,
    patchmax_proj_size_slack  = 0.05,
    patchmax_candidate_min_size = NULL,
    patchmax_st_seed = NULL,
    # patchmax_st_distance = NULL, # << TODO delete
    patchmax_SDW = NULL,
    patchmax_sample_n = NULL
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
    if(length(scenario_write_tags) > 1 & is.null(names(scenario_write_tags)) == FALSE){
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
        message(paste0("Output directory, ", absolute_output_path, ", already exists"))
        list.files(absolute_output_path, full.names = T) %>% file.remove()
      } else {
        message(paste0("Output directory, ", absolute_output_path, ", already exists"))
        if (overwrite_output) {
          list.files(absolute_output_path, full.names = T) %>% file.remove()
          message('...Overwriting previous files')
        }
      }
    }

    # !!!!!!!!!!!!!!!!!!!!!!!!!!!
    # 1. PREP STANDS !!!!!!!!!!!!
    # !!!!!!!!!!!!!!!!!!!!!!!!!!!

    # Load stand data
    if (!is.null(stand_data)) {
      message("Forsys Shiny data detected.")
      stands <- stand_data 
      stands <- stands %>% mutate(!!stand_id_field := as.character(get(stand_id_field)))
      data.table::setDT(stands)
    } else if(!is.null(stand_data_filename)) {
      message("Loading stand data from file")
      stands <- load_dataset(path_to_file = stand_data_filename)
      stands <- stands %>% mutate(!!stand_id_field := as.character(get(stand_id_field)))
      data.table::setDT(stands)
    } else {
      stop("No stand data provided")
    }

    # Load fire data
    if (run_with_fire & !is.null(fire_intersect_table)) {
      message("Forsys fire data detected")
      fires <- fire_intersect_table
      } else if(run_with_fire & !is.null(fire_intersect_table_filename)) {
      message("Loading fire data from file")
      fires <- data.table(foreign::read.dbf(fire_intersect_table_filename))
    }

    # filter stands by availability
    stands <- stands %>%
      filter_stands(filter_txt = global_threshold)

    # Calculate SPM & PCP values
    
    # assume stand_pcp_spm fields equal scenario priorities if NULL
    if(is.null(stand_pcp_spm)){
      stand_pcp_spm <- scenario_priorities
    }

    # calculate spm and spm for scenario priorities and scenario output fields
    stands <- stands %>%
      calculate_spm_pcp(fields = stand_pcp_spm, normalize = normalize_values) %>%
      calculate_spm_pcp(fields = scenario_output_fields, normalize = normalize_values)

    # set up weighting scenarios
    weights <- weight_priorities(
      numPriorities = length(scenario_priorities), 
      weights = scenario_weighting_values[1])

    # !!!!!!!!!!!!!!!!!!!!!!!!!!!
    # 2. LOOP THROUGH WEIGHTS !!!
    # !!!!!!!!!!!!!!!!!!!!!!!!!!!

    for (w in 1:nrow(weights)) { # START WEIGHT LOOP

      ## create the weighted priorities.
      message(paste0("\n---------------\nWeighting scenario ",  w,
                     " of ", nrow(weights),
                     ": ", paste0(weights[w,], collapse = '-')))

      # prep stand data
      stands <- stands %>%
        set_up_treatment_types() %>%
        set_up_priorities(
          w = w,
          priorities = paste0(scenario_priorities, '_SPM'),
          weights = weights)

      # manually add proj_id field if running with patchmax
      if(run_with_patchmax){
        stands <- stands %>% mutate(!!proj_id_field := NA)
      }

      # create objects for tracking treated and burnt stands
      stands_available <- stands
      stands_selected <- NULL
      stands_treated <- NULL
      stands_burned <- NULL

      # !!!!!!!!!!!!!!!!!!!!!!!!!!!
      # 2. SELECT STANDS !!!!!!!!!!
      # !!!!!!!!!!!!!!!!!!!!!!!!!!!

      for (y in 1:fire_planning_years) { # BEGIN YEAR LOOP

        if (fire_planning_years > 1) message(paste('\nYear', y, '\n---------------'))

        # !! 2a. DYNAMIC PROJECTS ------

        if(run_with_patchmax){ # run with PatchMax

          suppressMessages(suppressWarnings(require(Patchmax)))

          # TODO Test that stand_data contains geometry
          geom <- sf::st_as_sf(stands_available)
          
          # randomly select stands (for testing purposes)
          if(!is.null(patchmax_sample_n)){
            patchmax_st_seed <- sample(
              x = stands_available %>% pull(!!stand_id_field),
              size = patchmax_sample_n, 
              replace = F)
          }
          
          sdw = 1
          
          if(!is.null(proj_target_field)){ 
            P_constraint = pull(geom, !!proj_target_field)
          } else {
            P_constraint = NULL
            proj_target_value = Inf
          }
          
          patchmax_out <- patchmax::simulate_projects(
            geom = geom,
            St_id = pull(geom, !!stand_id_field), 
            St_area = pull(geom, !!stand_area_field), 
            St_objective = pull(geom, weightedPriority), 
            # St_seed = patchmax_st_seed,
            P_size = patchmax_proj_size, 
            P_size_slack  = patchmax_proj_size_slack, 
            P_number = patchmax_proj_number,
            St_threshold = stand_threshold,
            SDW = sdw,
            P_constraint = P_constraint,
            P_constraint_max_value = proj_target_value,
            P_constraint_min_value = proj_target_min_value,
            sample_frac = .1
          )

          # clean up output
          projects_selected_y <- patchmax_out[[1]] %>%
            rename(treatment_rank = Project,
                          weightedPriority = Objective) %>%
            mutate(!!proj_id_field := treatment_rank)

          stands_selected_y <- patchmax_out[[2]] %>%
            select(!!stand_id_field := Stands,
                          !!proj_id_field := Project,
                          treatment_rank := Project,
                          treated := DoTreat,
                          weightedPriority = Objective)

        } else { # run with preassigned (static) projects

          patchstat_out <- build_static_projects(
            stands = stands_available,
            stand_id_field = stand_id_field,
            stand_area_field = stand_area_field,
            proj_id_field = proj_id_field,
            proj_target_field = proj_target_field,
            proj_fixed_target = proj_fixed_target,
            proj_target_value = proj_target_value,
            proj_target_min_value = proj_target_min_value, # TODO Need to update apply_treatment to recognize this.
            stand_threshold = stand_threshold,
            proj_treatment_name = proj_treatment_name,
            proj_number = NULL,
            proj_area_ceiling = fire_annual_target
          )

          projects_selected_y <- patchstat_out[[1]]
          stands_selected_y <- patchstat_out[[2]]
        }

        # FIRE: randomize project rank if desired (research specific task)
        if(fire_random_projects){
          message('!! Randomizing projects')

          shuffled_weights <- projects_selected_y %>%
            filter(treatment_rank %>% is.na == FALSE) %>%
            mutate(weightedPriority = sample(weightedPriority)) %>%
            select(proj_id_field, weightedPriority)

           projects_selected_y <- projects_selected_y %>% select(-weightedPriority) %>%
             left_join(shuffled_weights, by = proj_id_field) %>%
             arrange(-weightedPriority) %>%
             mutate(treatment_rank = ifelse(weightedPriority > 0, 1:n(), NA)) %>%
             tidyr::drop_na(treatment_rank)
        }

        # !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
        # 4. UPDATE AVAILABILITY !!!!!!!!!!
        # !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

        # set annual target
        fire_annual_target_i = fire_annual_target[y]
        if(is.na(fire_annual_target_i)) {
          message("Assuming unlimited annual target")
          fire_annual_target_i = Inf # if no target available, set to Inf
        }

        if(is.null(fire_annual_target_field) == TRUE){ # assign all projects to year one if annual target is NULL
          projects_scheduled <- projects_selected_y %>% mutate(ETrt_YR = 1)
          stands_selected_y <- stands_selected_y %>% mutate(ETrt_YR = 1)
        } else if(is.null(fire_annual_target_field) == FALSE) { # assign project year based on annual target(s)
          projects_scheduled <- projects_selected_y %>%
            mutate(ETrt_YR = cumsum(get(fire_annual_target_field)) %/% !!fire_annual_target_i + 1) %>%
            mutate(ETrt_YR = ifelse(weightedPriority == 0, NA, ETrt_YR)) %>%
            filter(ETrt_YR == 1) %>%
            mutate(ETrt_YR = !!y)
          stands_selected_y <- stands_selected_y %>% inner_join(stand_id_field, ETrt_YR)
        }

        # record stands scheduled for treatment in current year
        stands_treated <- stands_selected_y %>%
          filter(treated == 1) %>%
          mutate(weighting_combo = w) %>%
          bind_rows(stands_treated)

        # record stands scheduled for treatment in current year
        stands_selected <- stands_selected_y %>%
          bind_rows(stands_selected)

        # remove stands or project areas that were treated from available stands
        x1 = unique(stands_selected_y[[stand_id_field]])
        stands_available <- stands_available %>% filter(.data[[stand_id_field]] %in% x1 == FALSE)

        # report yearly work
        s_n = stands_treated %>% filter(ETrt_YR == y) %>% pull(stand_id_field) %>% n_distinct()
        p_n = stands_treated %>% filter(ETrt_YR == y) %>% pull(treatment_rank) %>% n_distinct()
        message(paste0(s_n, ' stands (', round(s_n/nrow(stands) * 100, 2), '% of total) treated in ', p_n, ' projects'))

        # !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
        # 5. BEGIN ANNUAL FIRES !!!!!!!!!!!
        # !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

        if(run_with_fire & !is.null(fire_intersect_table)) {

          # record stands that burned this year
          stands_burned <- stands %>%
            left_join(projects_scheduled, by=proj_id_field) %>%
            left_join(stands_treated %>% select(stand_id_field), by=stand_id_field) %>%
            left_join(fires %>% select(stand_id_field, FIRE_YR, FIRE_NUMBER), by=stand_id_field) %>%
            filter(FIRE_YR == !!y) %>%
            select(stand_id_field, proj_id_field, ETrt_YR, FIRE_YR, FIRE_NUMBER, treatment_rank, weightedPriority) %>%
            bind_rows(stands_burned)

          # report yearly fire
          b_n = stands_burned %>% filter(FIRE_YR == y) %>% pull(stand_id_field) %>% n_distinct()
          message(paste0(b_n, ' (', round(b_n/nrow(stands) * 100, 2), '%) stands burned'))

          # remove burnt stands from future selection only if fire_dynamic_forsys is TRUE
          if(fire_dynamic_forsys == TRUE) {
            stands_available <- stands_available %>%
              filter(.data[[stand_id_field]] %in% stands_burned[[stand_id_field]] == FALSE)
          }

        } # END ANNUAL FIRES

      } # END YEAR LOOP


      # !!!!!!!!!!!!!!!!!!!!!!!!!!!
      # 5. WRITE DATA !!!!!!!!!!!!!
      # !!!!!!!!!!!!!!!!!!!!!!!!!!!

      # tag stands with specific scenario attributes
      stands_treated_out <- stands_treated %>%
                              select(!!stand_id_field, !!proj_id_field, ETrt_YR)

      # record fire information if provided
      if(run_with_fire & !is.null(fire_intersect_table)){
        stands_treated_out <- stands_treated_out %>%
          left_join(fires %>% select(stand_id_field, FIRE_YR, FIRE_NUMBER), by=stand_id_field)
      }

      # update project output fields to include all PCP fields if present
      pcp_fields <- select(stands, matches('_PCP$')) %>% names()
      scenario_output_fields <- c(scenario_output_fields, pcp_fields)

      # ........................................
      # write stands to file ...................
      # ........................................

      # add scenario tag to output
      stands_treated_out <- stands_treated_out %>%
        bind_cols(scenario_write_tags)

      stands_treated_out <- stands_treated_out %>%
        left_join(
          y= stands %>% select(!!stand_id_field, scenario_output_fields),
          by = stand_id_field)

      # assign weight scenario values to stand out out
      stands_treated_out[,paste0('Pr_', 1:length(scenario_priorities), '_', scenario_priorities)] = weights[w,]

      # generate output filename
      if (length(scenario_write_tags_txt) > 1) {
        stand_fn <- paste0(relative_output_path, "/stnd_", scenario_name, '_', scenario_write_tags_txt, ".csv")
      } else {
        stand_fn <- paste0(relative_output_path, "/stnd_", scenario_name, ".csv")
      }

      # write data
      data.table::fwrite(stands_treated_out, stand_fn, row.names = FALSE, append = TRUE)

      # ........................................
      # write project data to file .............
      # ........................................

      # summarize selected stands by grouping fields and tag with ETrt_ prefix
      projects_etrt_out <- stands_treated_out  %>%
        select(stand_id_field, proj_id_field, ETrt_YR) %>%
        left_join(stands %>% select(stand_id_field, scenario_output_grouping_fields, 
                                                  scenario_output_fields, weightedPriority),
                         by = stand_id_field, suffix = c("", ".dup")) %>%
        create_grouped_dataset(
          grouping_vars = unique(c(proj_id_field, scenario_output_grouping_fields, 'ETrt_YR')),
          summing_vars = c(scenario_output_fields, 'weightedPriority')
          ) %>%
        arrange(ETrt_YR, -weightedPriority) %>%
        dplyr::rename_with(.fn = ~ paste0("ETrt_", .x), .cols = scenario_output_fields) %>%
        base::replace(is.na(.), 0)

      # summarize available stands by grouping fields and tag with ESum_ prefix
      projects_esum_out <- stands_selected %>%
        select(stand_id_field, proj_id_field) %>%
        left_join(stands %>% select(stand_id_field, scenario_output_grouping_fields, 
                                                  scenario_output_fields, weightedPriority),
                         by = stand_id_field, suffix = c("", ".dup")) %>%
        compile_planning_areas_and_stands(
          unique_weights = uniqueWeights,
          group_by = c(proj_id_field, scenario_output_grouping_fields),
          output_fields = scenario_output_fields)

      # join etrt w/ esum outputs
      projects_etrt_esum_out <- projects_etrt_out %>%
        inner_join(projects_esum_out, by=unique(c(proj_id_field, scenario_output_grouping_fields))) %>%
        base::replace(is.na(.), 0)

      # rank projects
      projects_rank <- projects_etrt_out %>%
        dplyr::group_by(!!proj_id_field := get(proj_id_field)) %>%
        dplyr::summarize_at('weightedPriority', sum) %>%
        arrange(-weightedPriority) %>%
        mutate(treatment_rank = rank(-weightedPriority)) %>%
        select(!!proj_id_field, treatment_rank)

      # tag weighting scenario
      priority_write_tags <- weights[w,] %>%
        setNames(paste0('Pr_', 1:length(scenario_priorities), '_', scenario_priorities))

      # tag subset output with treatment rank, scenario_write_tags, priority weights
      subset_out <- projects_etrt_esum_out %>%
        left_join(projects_rank, by = proj_id_field) %>%
        arrange(treatment_rank) %>%
        bind_cols(scenario_write_tags) %>%
        bind_cols(priority_write_tags)

      # tag project output with treatment rank, scenario_write_tags, priority weights
      projects_out <- projects_etrt_esum_out %>%
        dplyr::group_by(!!proj_id_field := get(proj_id_field), ETrt_YR) %>%
        dplyr::summarize_if(is.numeric, sum) %>%
        left_join(projects_rank, by = proj_id_field) %>%
        arrange(treatment_rank) %>%
        bind_cols(scenario_write_tags) %>%
        bind_cols(priority_write_tags)

      # write tag for selection scenario
      if (length(scenario_write_tags_txt) > 1) {
        project_fn = paste0(relative_output_path, "/proj_", scenario_name,  '_', scenario_write_tags_txt, ".csv")
        subset_fn = paste0(relative_output_path, "/subset_", scenario_name,  '_', scenario_write_tags_txt, ".csv")
      } else {
        project_fn = paste0(relative_output_path, "/proj_", scenario_name, ".csv")
        subset_fn = paste0(relative_output_path, "/subset_", scenario_name, ".csv")
      }

      projects_out %>% dplyr::ungroup() %>% select(matches('Pr_[0-9]_')) %>% apply(1, paste0, collapse='_') %>% unique()

      # write out project data
      data.table::fwrite(projects_out, file = project_fn, sep = ",", row.names = FALSE, append = TRUE)
      data.table::fwrite(subset_out, file = subset_fn, sep = ",", row.names = FALSE, append = TRUE)

    }
    # END WEIGHT LOOP

    if(return_outputs){
      return(list(
        stand_output = data.table::fread(stand_fn),
        project_output = data.table::fread(project_fn),
        subset_output = data.table::fread(subset_fn)
      ))
    }
    message('Forsys simulation is complete')
}
# END RUN
