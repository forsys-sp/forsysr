#' ForSysR
#'
#' Primary function for running the ForSys treatment planner. Either provide 
#' parameters, or define parameters in a config file and pass the name of the 
#' file to this run function.
#'
#' @param config_file Config file that defines `forsys::run` parameters. <\emph{character}> 
#' @param return_outputs Return project and stand directly. <\emph{logical}> 
#' @param write_outputs Write project and stand data to file. <\emph{logical}> 
#' @param overwrite_output Overwrites existing output of the same name. <\emph{logical}> 
#' @param run_with_shiny Whether run was called from within shiny. <\emph{logical}> 
#' @param run_with_fire Whether to forsys alongside fire. <\emph{logical}> 
#' @param run_with_patchmax Whether PatchMax should be used for building projects. <\emph{logical}> 
#' @param stand_data Stand dataset. <\emph{data.frame, data.table, sf}> 
#' @param stand_data_filename Path to the input dataset saved as CSV file. <\emph{character}> 
#' @param stand_id_field Field name containing unique ID for each stand. <\emph{character}> 
#' @param stand_area_field Field name containing stand area. <\emph{character}> 
#' @param global_threshold Boolean statement used to specify which stands are within the scenario. <\emph{character}> 
#' @param proj_id_field Field name indicating which planning area a stand belongs to. <\emph{character}> 
#' @param stand_threshold Boolean statement defining stands counted towards project objective <\emph{character}> 
#' @param proj_fixed_target Whether target is fixed or relative. <\emph{logical}> 
#' @param proj_target_field Field name used as target constraint. <\emph{character}>
#' @param proj_target_value Target constraint: fixed value if `proj_fixed_target` is TRUE or 0 and 1 if FALSE. <\emph{numeric}> 
#' @param proj_target_min_value Minimum valid target constraint. Only used if `run_with_patchmax` is TRUE.
#' @param planning_years Number of years to run forsys. <\emph{integer}> 
#' @param annual_target_field Field name to use for calculating annual target. <\emph{character}> 
#' @param annual_target Value of annual cumulative target. <\emph{numeric}> 
#' @param scenario_name Name for this scenario. <\emph{character}> 
#' @param scenario_priorities Scenario priorities. <\emph{character vector}> 
#' @param scenario_weighting_values String of 3 integers separated by spaces defining weighting min, max, and step. <\emph{character}> 
#' @param scenario_output_fields Field names to write out. <\emph{character vector}>
#' @param scenario_output_grouping_fields Field names for grouping of treated stands. <\emph{character vector}> 
#' @param scenario_write_tags String appended to output used to describe scenario. <\emph{optional character}> 
#' @param fire_intersect_table Stands affected by fire by year. <\emph{data.frame}> 
#' @param fire_intersect_table_filename File name of csv listing stands affected by fire by year. <\emph{character}> 
#' @param fire_dynamic_forsys Whether burnt stands are prevented from being selected. <\emph{logical}> 
#' @param fire_random_projects Whether project prioritization is randomly shuffled. <\emph{logical}> 
#' @param patchmax_proj_number Number of projects to build with patchmax. <\emph{integer}> 
#' @param patchmax_proj_size Target area for each patchmax project. <\emph{integer}> 
#' @param patchmax_proj_size_min Minimum valid project size when using patchmax. <\emph{numeric}> 
#' @param patchmax_sample_frac Percent of stands to search. <\emph{numeric 0-1}> 
#' @param patchmax_st_seed Specific stand IDs to search. <\emph{numeric/character vector}> 
#' @param patchmax_SDW Stand distance weight parameter. Default is 0.5. <\emph{numeric 0-1}> 
#' @param patchmax_EPW Stand exclusion weight parameter. Default is 0.5. <\emph{numeric 0-1}> 
#'
#' @return list with selected stands, project summary, project summary by subgroup
#'
#' @importFrom rlang .data
#' @importFrom dplyr %>% select mutate rename arrange filter left_join inner_join bind_rows bind_cols n n_distinct group_by summarize summarize_at summarize_if
#'
#' @export
run <- function(
    config_file = NULL,
    return_outputs = FALSE,
    write_outputs = TRUE,
    overwrite_output = TRUE,
    # basic
    stand_data = NULL,
    stand_data_filename = NULL,
    stand_id_field = NULL,
    stand_area_field = NULL,
    stand_threshold = NULL,
    global_threshold = NULL,
    # project variables
    proj_id_field = NULL,
    proj_fixed_target = TRUE,
    proj_target_field = NULL,
    proj_target_value = NULL,
    proj_target_min_value = -Inf,
    # annual targets
    planning_years = 1,
    annual_target_field = NULL,
    annual_target = NA,
    # scenario variables
    scenario_name = NULL,
    scenario_priorities = NULL,
    scenario_weighting_values = "1 1 1", # TODO separate to 3 parameters
    scenario_output_fields = NULL,
    scenario_output_grouping_fields = NULL,
    scenario_write_tags = NULL,
    # module toggles
    run_with_shiny = FALSE,
    run_with_patchmax = FALSE,
    run_with_fire = FALSE,
    # fire arguments
    fire_intersect_table = NULL,
    fire_intersect_table_filename = NULL,
    fire_dynamic_forsys = FALSE,
    fire_random_projects = FALSE,
    # patchmax arguments
    patchmax_proj_number = 1,
    patchmax_proj_size = Inf,
    patchmax_proj_size_min = -Inf,
    patchmax_sample_frac = 0.1,
    patchmax_st_seed = NULL,
    patchmax_SDW = 0.5,
    patchmax_EPW = 0.5
    ) {
  
    # !!!!!!!!!!!!!!!!!!!!!!!!!!!
    # 0. SETUP !!!!!!!!!!!!!!!!!!
  
    options(scipen = 9999)
  
    # source function parameters from config file if provided
    if (length(config_file) > 0) {
      if (stringr::str_detect(config_file, '[.]R$')) {
        source(config_file, local = TRUE)
      }
      if (stringr::str_detect(config_file, '[.]json$')) {
        load_json_config(config_file)
      }
    }
  
    # collapse write tags into string if provided as data.frame
    if (length(scenario_write_tags) > 1 & !is.null(names(scenario_write_tags))) {
      scenario_write_tags_txt <- paste(names(scenario_write_tags), scenario_write_tags, sep='_', collapse = '_')
    } else {
      scenario_write_tags_txt <- scenario_write_tags
    }

    if (write_outputs) {
      # create output directory
      relative_output_path = paste0('output/', scenario_name, '/', scenario_write_tags_txt)
      create_output_directory(relative_output_path, run_with_shiny, overwrite_output)
      
      # save input parameters to file
      params <- ls()[grepl('stand_data', ls()) == FALSE]
      input_params <- sapply(params, function(x){tryCatch(get(x), error = function(e) return(0))})
      writeLines(jsonlite::toJSON(input_params, pretty = TRUE), 
                 paste0(relative_output_path, '/', scenario_name, '.json'))
    }

    # !!!!!!!!!!!!!!!!!!!!!!!!!!!
    # 1. PREP STANDS !!!!!!!!!!!!

    # Load stand data 
    if (!is.null(stand_data)) {
      stands <- stand_data 
      stands <- stands %>% mutate(!!stand_id_field := as.character(get(stand_id_field)))
    } else if (!is.null(stand_data_filename)) {
      message("Loading stand data from file")
      stands <- load_dataset(path_to_file = stand_data_filename)
      stands <- stands %>% mutate(!!stand_id_field := as.character(get(stand_id_field)))
    } else {
      stop("Stand data required")
    }

    # Load fire data
    if (run_with_fire & !is.null(fire_intersect_table)) {
      fires <- fire_intersect_table
      } else if (run_with_fire & !is.null(fire_intersect_table_filename)) {
      message("Loading fire data from file")
      fires <- data.table(foreign::read.dbf(fire_intersect_table_filename))
    }

    # filter stands by availability
    stands <- stands %>% filter_stands(filter_txt = global_threshold)

    # set up weighting scenarios
    weights <- weight_priorities(
      numPriorities = length(scenario_priorities), 
      weights = scenario_weighting_values[1])
    
    # reserve key output data
    stands_out <- NULL
    projects_out <- NULL
    subset_out <- NULL

    # !!!!!!!!!!!!!!!!!!!!!!!!!!!
    # 2. LOOP THROUGH WEIGHTS !!!

    for (w in 1:nrow(weights)) { # START WEIGHT LOOP

      ## create the weighted priorities.
      message(paste0("Weighting scenario ", w, " of ", nrow(weights), ": ", paste0(weights[w,], collapse = '-')))

      # prep stand data
      stands <- stands %>%
        mutate(
          DoTreat = 0,
          selected = 0,
          proj_target_treated = 0,
          weightedPriority = 0
        ) %>%
        combine_priorities(
          fields = scenario_priorities,
          weights = unlist(weights[w,]),
          new_field = 'weightedPriority',
          append_weights = TRUE
        )
      
      # manually add proj_id field if running with patchmax
      if (run_with_patchmax) {
        stands <- stands %>% mutate(!!proj_id_field := NA)
      }

      # create objects for tracking treated and burnt stands
      stands_available <- stands
      stands_selected <- NULL
      stands_burned <- NULL

      # !!!!!!!!!!!!!!!!!!!!!!!!!!!
      # 2. SELECT STANDS !!!!!!!!!!

      for (y in 1:planning_years) { # BEGIN YEAR LOOP

        if (planning_years > 1) {
          message(paste('\nYear', y, '\n---------------'))
        }

        if (run_with_patchmax == TRUE) {

          suppressMessages(suppressWarnings(require(Patchmax)))
          geom <- sf::st_as_sf(stands_available)
          
          if (!is.null(proj_target_field)) { 
            P_constraint = pull(geom, !!proj_target_field)
          } else {
            P_constraint = NULL
            proj_target_value = Inf
          }
          
          # run with dynamic planning areas (i.e., patches)
          patchmax_out <- patchmax::simulate_projects(
            geom = geom,
            St_id = pull(geom, !!stand_id_field), 
            St_area = pull(geom, !!stand_area_field), 
            St_objective = pull(geom, weightedPriority), 
            P_size = patchmax_proj_size, 
            P_size_min  = patchmax_proj_size_min, 
            P_number = patchmax_proj_number,
            St_threshold = stand_threshold,
            SDW = patchmax_SDW,
            EPW = patchmax_EPW,
            P_constraint = P_constraint,
            P_constraint_max_value = proj_target_value,
            P_constraint_min_value = proj_target_min_value,
            sample_frac = patchmax_sample_frac
          )

          projects_selected_y <- patchmax_out[[1]] %>%
            rename(treatment_rank = Project, weightedPriority = Objective) %>%
            mutate(!!proj_id_field := treatment_rank)

          stands_selected_y <- patchmax_out[[2]] %>%
            select(!!stand_id_field := Stands,
                   !!proj_id_field := Project,
                   treatment_rank := Project,
                   DoTreat := DoTreat,
                   weightedPriority = Objective) %>%
            mutate(selected = 1)
        } 
        
        # run with predetermined planning areas
        if (run_with_patchmax == FALSE) {

          patchstat_out <- build_static_projects(
            stands = stands_available,
            stand_id_field = stand_id_field,
            stand_area_field = stand_area_field,
            proj_id_field = proj_id_field,
            proj_target_field = proj_target_field,
            proj_fixed_target = proj_fixed_target,
            proj_target_value = proj_target_value,
            proj_target_min_value = proj_target_min_value,
            stand_threshold = stand_threshold,
            proj_number = NULL,
            proj_area_ceiling = annual_target
          )

          projects_selected_y <- patchstat_out[[1]]
          stands_selected_y <- patchstat_out[[2]]
        }

        # !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
        # 4. UPDATE AVAILABILITY !!!!!!!!!!

        # set annual target
        annual_target_i = annual_target[y]
        if (is.na(annual_target_i)) {
          message("Assuming unlimited annual target")
          annual_target_i = Inf
        }
        
        # assign all projects to year one if annual target is NULL
        if (is.null(annual_target_field) == TRUE) { 
          projects_scheduled <- projects_selected_y %>% mutate(ETrt_YR = 1)
          stands_selected_y <- stands_selected_y %>% mutate(ETrt_YR = 1)
        } 
        
        # assign project year based on annual target(s)
        if (is.null(annual_target_field) == FALSE) { 
          projects_scheduled <- projects_selected_y %>%
            mutate(ETrt_YR = cumsum(get(annual_target_field)) %/% !!annual_target_i + 1) %>%
            mutate(ETrt_YR = ifelse(weightedPriority == 0, NA, ETrt_YR)) %>%
            filter(ETrt_YR == 1) %>%
            mutate(ETrt_YR = !!y)
          stands_selected_y <- stands_selected_y %>% inner_join(stand_id_field, ETrt_YR)
        }
        
        # record stands scheduled for treatment in current year
        stands_selected <- stands_selected_y %>% 
          mutate(weighting_combo = w) %>%
          bind_rows(stands_selected)
        
        # remove stands or project areas that were treated from available stands
        x1 <- unique(stands_selected_y[[stand_id_field]])
        stands_available <- stands_available %>% filter(.data[[stand_id_field]] %in% x1 == FALSE)

        # report yearly work
        s_n <- stands_selected %>% filter(ETrt_YR == y, DoTreat == 1) %>% pull(stand_id_field) %>% n_distinct()
        p_n <- stands_selected %>% filter(ETrt_YR == y, DoTreat == 1) %>% pull(treatment_rank) %>% n_distinct()
        message(paste0(s_n, ' stands (', round(s_n/nrow(stands) * 100, 2), '% of total) treated in ', 
                       p_n, ' projects (total objective: ', round(sum(stands_selected$weightedPriority),2), ')'))

        # !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
        # 5. BEGIN ANNUAL FIRES !!!!!!!!!!!

        if (run_with_fire & is.null(fire_intersect_table) == FALSE) { # BEGIN FIRE LOOP

          select_fields <- c(stand_id_field, proj_id_field, ETrt_YR, FIRE_YR, 
                             FIRE_NUMBER, treatment_rank, weightedPriority)
          
          # record stands that burned this year
          stands_burned <- stands %>%
            left_join(projects_scheduled, by=proj_id_field) %>%
            left_join(stands_selected %>% filter(DoTreat == 1) %>% select(stand_id_field), by=stand_id_field) %>%
            left_join(fires %>% select(stand_id_field, FIRE_YR, FIRE_NUMBER), by=stand_id_field) %>%
            select(any_of(select_fields)) %>% 
            filter(FIRE_YR == !!y) %>%
            bind_rows(stands_burned)

          # report yearly fire
          b_n <- stands_burned %>% filter(FIRE_YR == y) %>% pull(stand_id_field) %>% n_distinct()
          message(paste0(b_n, ' (', round(b_n/nrow(stands) * 100, 2), '%) stands burned'))

          # remove burnt stands from future selection only if fire_dynamic_forsys is TRUE
          if (fire_dynamic_forsys == TRUE) {
            stands_available <- stands_available %>% 
              filter(.data[[stand_id_field]] %in% stands_burned[[stand_id_field]] == FALSE)
          }

        } # END FIRE LOOP

      } # END YEAR LOOP

      # !!!!!!!!!!!!!!!!!!!!!!!!!!!
      # 5. SUMMARIZE DATA !!!!!!!!!

      # append area to output fields if available
      scenario_output_fields <- unique(stand_area_field, scenario_output_fields)
      
      # tag stands with specific scenario attributes
      stands_out_w <- stands_selected %>% 
        # filter(DoTreat == 1) %>%
        select(!!stand_id_field, !!proj_id_field, ETrt_YR, DoTreat, selected)

      # record fire information if provided
      if (run_with_fire & !is.null(fire_intersect_table)) {
        stands_out_w <- stands_out_w %>%
          left_join(fires %>% select(stand_id_field, FIRE_YR, FIRE_NUMBER), by=stand_id_field)
      }

      # add scenario tag to output
      stands_out_w <- stands_out_w %>% bind_cols(scenario_write_tags)

      # add scenario output fields to stand output
      join_y <- stands %>% select(!!stand_id_field, any_of(scenario_output_fields))
      stands_out_w <- left_join(stands_out_w, join_y, by = stand_id_field)

      # assign weight scenario values to stand out out
      stands_out_w[, paste0('Pr_', 1:length(scenario_priorities), '_', scenario_priorities)] = weights[w,]

      # summarize project data
      summary_out <- summarize_projects(
        selected_stands = stands_out_w,
        stands_data = stands,
        stand_id_field = stand_id_field,
        proj_id_field = proj_id_field,
        scenario_output_grouping_fields = scenario_output_grouping_fields,
        scenario_output_fields = scenario_output_fields
      )

      # tag weighting scenario
      priority_write_tags <- as.data.frame(weights[w,]) %>%
        setNames(paste0('Pr_', 1:length(scenario_priorities), '_', scenario_priorities))

      # tag subset output with treatment rank, scenario_write_tags, priority weights
      subset_out_w <- summary_out %>%
        arrange(treatment_rank) %>%
        bind_cols(scenario_write_tags) %>%
        bind_cols(priority_write_tags)

      # tag project output with treatment rank, scenario_write_tags, priority weights
      projects_out_w <- summary_out %>%
        group_by(!!proj_id_field := get(proj_id_field), ETrt_YR) %>%
        summarize_if(is.numeric, sum) %>%
        arrange(treatment_rank) %>%
        bind_cols(scenario_write_tags) %>%
        bind_cols(priority_write_tags)
      
      stands_out <- bind_rows(stands_out, stands_out_w)
      projects_out <- bind_rows(projects_out, projects_out_w)
      subset_out <- bind_rows(subset_out, subset_out_w)
      
    }  # END WEIGHT LOOP
    
    # !!!!!!!!!!!!!!!!!!!!!!!!!!!
    # 6. WRITE DATA !!!!!!!!!!!!!
    # !!!!!!!!!!!!!!!!!!!!!!!!!!!
    
    if (write_outputs) {
        
      # create output filename
      if (length(scenario_write_tags_txt) > 1) {
        stand_fn <- paste0(relative_output_path, "/stnd_", scenario_name, '_', scenario_write_tags_txt, ".csv")
        project_fn = paste0(relative_output_path, "/proj_", scenario_name,  '_', scenario_write_tags_txt, ".csv")
        subset_fn = paste0(relative_output_path, "/subset_", scenario_name,  '_', scenario_write_tags_txt, ".csv")
      } else {
        stand_fn <- paste0(relative_output_path, "/stnd_", scenario_name, ".csv")
        project_fn = paste0(relative_output_path, "/proj_", scenario_name, ".csv")
        subset_fn = paste0(relative_output_path, "/subset_", scenario_name, ".csv")
      }
    
      # remove list from output data (case when working with sf data)
      stands_out <- stands_out %>% select(!tidyselect::where(is.list))
      
      # write stand data data
      message(paste0('Scenario output data written to: ', relative_output_path))
      data.table::fwrite(stands_out, stand_fn, row.names = FALSE)
      data.table::fwrite(projects_out, file = project_fn, sep = ",", row.names = FALSE)
      data.table::fwrite(subset_out, file = subset_fn, sep = ",", row.names = FALSE)
    }

    if (return_outputs) {
      return(list(
        stand_output = stands_out,
        project_output = projects_out,
        subset_output = subset_out
      ))
    }
    message('Forsys simulation is complete')
} # END FUNCTION
