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
#' @param return_outputs 
#'
#' @return Forsys results with weightedPriority, treatmentRank, and weights
#'
#' @importFrom rlang .data
#' @importFrom dplyr %>%
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
    proj_target_value = NULL,
    proj_treatment_name = "",
    # scenario variables
    scenario_name = "",
    scenario_priorities = NULL,
    scenario_weighting_values = "1 1 1",
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
    # patchmax arguments
    patchmax_stnd_adj = NULL,
    patchmax_stnd_adj_filename = NULL,
    patchmax_proj_number = 1,
    patchmax_proj_size = Inf,
    patchmax_proj_size_slack  = 0.05,
    patchmax_candidate_min_size = NULL,
    patchmax_st_seed = NULL,
    patchmax_st_distance = NULL,
    patchmax_SDW = NULL,
    pathmax_sample_n = NULL,
    pathmax_sample_seed = 12345
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
    # 1. PREP STANDS ------------
    # !!!!!!!!!!!!!!!!!!!!!!!!!!!

    # Load stand data
    if (!is.null(stand_data)) {
      message("Forsys Shiny data detected.")
      stands <- stand_data
      data.table::setDT(stands)
    } else if(!is.null(stand_data_filename)) {
      message("Loading stand data from file")
      stands <- load_dataset(path_to_file = stand_data_filename)
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

    # Load patchmax adjacency data
    if (run_with_patchmax & !is.null(patchmax_stnd_adj)) {
      message("Forsys stand adjacency data detected")
      adj_object <- patchmax_stnd_adj
    } else if(run_with_patchmax & !is.null(patchmax_stnd_adj_filename)) {
      message("Loading stand adjacency data from file")
      # read edge list; force to characters
      edgelist <- read.csv(patchmax_stnd_adj_filename) %>%
        as.matrix() %>%
        apply(MARGIN = 2, FUN = as.character)
      adj_object <- igraph::graph_from_edgelist(edgelist)
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
    # 2. LOOP THROUGH WEIGHTS ---
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
        stands <- stands %>% dplyr::mutate(!!proj_id_field := NA)
      }

      # create objects for tracking treated and burnt stands
      stands_available <- stands
      stands_selected <- NULL
      stands_treated <- NULL
      stands_burned <- NULL

      # !!!!!!!!!!!!!!!!!!!!!!!!!!!
      # 2. SELECT STANDS ----------
      # !!!!!!!!!!!!!!!!!!!!!!!!!!!

      for (y in 1:fire_planning_years) { # BEGIN YEAR LOOP

        if (fire_planning_years > 1) message(paste('\nYear', y, '\n---------------'))

        # !! 2a. DYNAMIC PROJECTS ------

        if(run_with_patchmax){ # run with PatchMax

          suppressMessages(suppressWarnings(require(Patchmax)))

          # fail if no adjacency object
          if(is.null(adj_object)){
            stop('Patchmax requires an adjacency igraph object')
          }

          # helper for translating stand threshold into fields
          threshold_dat <- Patchmax::forsys_helper_prep_threshold(stand_threshold)
          threshold_field <- threshold_dat$st_threshold
          threshold_value <- threshold_dat$st_threshold_value
          if(proj_fixed_target == FALSE){
            stop('Patchmax requires that proj_fixed_target == TRUE')
          }
          
          # randomly select stands (for testing purposes)
          if(!is.null(pathmax_sample_n)){
            st_id <- stands_available %>% dplyr::pull(!!stand_id_field)
            patchmax_st_seed <- sample(st_id, pathmax_sample_n, F)
          }
          
          # extract project constraint values if proj_target_field provided
          proj_target_values <- NULL
          if(!is.null(proj_target_field)){
            proj_target_values <- stands[,get(proj_target_field)]
          } 
          
          # run patchmax
          patchmax_out <- Patchmax::simulate_projects(
            St_id = stands_available %>% dplyr::pull(!!stand_id_field), # stand id vector
            St_adj = adj_object,
            St_area = stands_available %>% dplyr::pull(!!stand_area_field), # stand area vector
            St_objective = stands_available %>% dplyr::pull(weightedPriority), # vector of stand values to maximize
            St_seed = patchmax_st_seed,
            P_size = patchmax_proj_size, # numeric project size constraint based on St_area
            P_size_slack  = patchmax_proj_size_slack,  # 0-1 numeric setting flexibility in hitting P_size constraint
            # P_size_ceiling = Inf,
            P_number = patchmax_proj_number, # integer count of projects to create
            St_threshold = stands_available %>% dplyr::pull(!!threshold_field),
            St_threshold_value = threshold_value,
            St_distance = patchmax_st_distance,
            SDW = patchmax_SDW,
            P_constraint = proj_target_values, # secondary constraint values
            P_constraint_max_value = proj_target_value,
            P_constraint_min_value = -Inf, # minimum secondary project constraint
            Candidate_min_size = 50
          )

          # clean up output
          projects_selected_y <- patchmax_out[[1]] %>%
            dplyr::rename(treatment_rank = Project,
                          weightedPriority = Objective) %>%
            dplyr::mutate(!!proj_id_field := treatment_rank)

          stands_selected_y <- patchmax_out[[2]] %>%
            dplyr::select(!!stand_id_field := Stands,
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
            dplyr::filter(treatment_rank %>% is.na == FALSE) %>%
            dplyr::mutate(weightedPriority = sample(weightedPriority)) %>%
            dplyr::select(proj_id_field, weightedPriority)

           projects_selected_y <- projects_selected_y %>% dplyr::select(-weightedPriority) %>%
             dplyr::left_join(shuffled_weights, by = proj_id_field) %>%
             dplyr::arrange(-weightedPriority) %>%
             dplyr::mutate(treatment_rank = ifelse(weightedPriority > 0, 1:dplyr::n(), NA)) %>%
             tidyr::drop_na(treatment_rank)
        }

        # !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
        # 4. UPDATE AVAILABILITY ----------
        # !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

        # set annual target
        fire_annual_target_i = fire_annual_target[y]
        if(is.na(fire_annual_target_i)) {
          message("Assuming unlimited annual target")
          fire_annual_target_i = Inf # if no target available, set to Inf
        }

        if(is.null(fire_annual_target_field) == TRUE){ # assign all projects to year one if annual target is NULL
          projects_scheduled <- projects_selected_y %>% dplyr::mutate(ETrt_YR = 1)
          stands_selected_y <- stands_selected_y %>% dplyr::mutate(ETrt_YR = 1)
        } else if(is.null(fire_annual_target_field) == FALSE) { # assign project year based on annual target(s)
          projects_scheduled <- projects_selected_y %>%
            dplyr::mutate(ETrt_YR = cumsum(get(fire_annual_target_field)) %/% !!fire_annual_target_i + 1) %>%
            dplyr::mutate(ETrt_YR = ifelse(weightedPriority == 0, NA, ETrt_YR)) %>%
            dplyr::filter(ETrt_YR == 1) %>%
            dplyr::mutate(ETrt_YR = !!y)
          stands_selected_y <- stands_selected_y %>% dplyr::inner_join(stand_id_field, ETrt_YR)
        }

        # record stands scheduled for treatment in current year
        stands_treated <- stands_selected_y %>%
          dplyr::filter(treated == 1) %>%
          dplyr::mutate(weighting_combo = w) %>%
          dplyr::bind_rows(stands_treated)

        # record stands scheduled for treatment in current year
        stands_selected <- stands_selected_y %>%
          dplyr::bind_rows(stands_selected)

        # remove stands or project areas that were treated from available stands
        x1 = unique(stands_selected_y[[stand_id_field]])
        stands_available <- stands_available %>% dplyr::filter(.data[[stand_id_field]] %in% x1 == FALSE)

        # report yearly work
        s_n = stands_treated %>% dplyr::filter(ETrt_YR == y) %>% dplyr::pull(stand_id_field) %>% dplyr::n_distinct()
        p_n = stands_treated %>% dplyr::filter(ETrt_YR == y) %>% dplyr::pull(treatment_rank) %>% dplyr::n_distinct()
        message(paste0(s_n, ' stands (', round(s_n/nrow(stands) * 100, 2), '% of total) treated in ', p_n, ' projects'))

        # !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
        # 5. BEGIN ANNUAL FIRES ----------
        # !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

        if(run_with_fire & !is.null(fire_intersect_table)) {

          # record stands that burned this year
          stands_burned <- stands %>%
            dplyr::left_join(projects_scheduled, by=proj_id_field) %>%
            dplyr::left_join(stands_treated %>% dplyr::select(stand_id_field), by=stand_id_field) %>%
            dplyr::left_join(fires %>% dplyr::select(stand_id_field, FIRE_YR, FIRE_NUMBER), by=stand_id_field) %>%
            dplyr::filter(FIRE_YR == !!y) %>%
            dplyr::select(stand_id_field, proj_id_field, ETrt_YR, FIRE_YR, FIRE_NUMBER, treatment_rank, weightedPriority) %>%
            dplyr::bind_rows(stands_burned)

          # report yearly fire
          b_n = stands_burned %>% dplyr::filter(FIRE_YR == y) %>% dplyr::pull(stand_id_field) %>% dplyr::n_distinct()
          message(paste0(b_n, ' (', round(b_n/nrow(stands) * 100, 2), '%) stands burned'))

          # remove burnt stands from future selection only if fire_dynamic_forsys is TRUE
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
      stands_treated_out <- stands_treated %>%
                              dplyr::select(!!stand_id_field, !!proj_id_field, ETrt_YR)

      # record fire information if provided
      if(run_with_fire & !is.null(fire_intersect_table)){
        stands_treated_out <- stands_treated_out %>%
          dplyr::left_join(fires %>% dplyr::select(stand_id_field, FIRE_YR, FIRE_NUMBER), by=stand_id_field)
      }

      # update project output fields to include all PCP fields if present
      pcp_fields <- dplyr::select(stands, matches('_PCP$')) %>% names()
      scenario_output_fields <- c(scenario_output_fields, pcp_fields)

      # ........................................
      # write stands to file ...................
      # ........................................

      # add scenario tag to output
      stands_treated_out <- stands_treated_out %>%
        dplyr::bind_cols(scenario_write_tags)

      stands_treated_out <- stands_treated_out %>%
        dplyr::left_join(
          y= stands %>% dplyr::select(!!stand_id_field, scenario_output_fields),
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
        dplyr::select(stand_id_field, proj_id_field, ETrt_YR) %>%
        dplyr::left_join(stands %>% dplyr::select(stand_id_field, scenario_output_grouping_fields, 
                                                  scenario_output_fields, weightedPriority),
                         by = stand_id_field, suffix = c("", ".dup")) %>%
        create_grouped_dataset(
          grouping_vars = unique(c(proj_id_field, scenario_output_grouping_fields, 'ETrt_YR')),
          summing_vars = c(scenario_output_fields, 'weightedPriority')
          ) %>%
        dplyr::arrange(ETrt_YR, -weightedPriority) %>%
        dplyr::rename_with(.fn = ~ paste0("ETrt_", .x), .cols = scenario_output_fields) %>%
        base::replace(is.na(.), 0)

      # summarize available stands by grouping fields and tag with ESum_ prefix
      projects_esum_out <- stands_selected %>%
        dplyr::select(stand_id_field, proj_id_field) %>%
        dplyr::left_join(stands %>% dplyr::select(stand_id_field, scenario_output_grouping_fields, 
                                                  scenario_output_fields, weightedPriority),
                         by = stand_id_field, suffix = c("", ".dup")) %>%
        compile_planning_areas_and_stands(
          unique_weights = uniqueWeights,
          group_by = c(proj_id_field, scenario_output_grouping_fields),
          output_fields = scenario_output_fields)

      # join etrt w/ esum outputs
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
        dplyr::group_by(!!proj_id_field := get(proj_id_field), ETrt_YR) %>%
        dplyr::summarize_if(is.numeric, sum) %>%
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

      projects_out %>% dplyr::ungroup() %>% dplyr::select(matches('Pr_[0-9]_')) %>% apply(1, paste0, collapse='_') %>% unique()

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
