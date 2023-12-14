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
#' @param annual_target_value Value of annual cumulative target. <\emph{numeric}> 
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
#' @param patchmax_sample_seed RNG seed used for random sampling <\emph{numeric or NULL}> 
#' @param patchmax_SDW Stand distance weight parameter. Default is 0.5. <\emph{numeric 0-1}> 
#' @param patchmax_EPW Stand exclusion weight parameter. Default is 0.5. <\emph{numeric 0-1}> 
#' @param patchmax_exclusion_limit Max percent of excluded stands in valid patch. <\emph{numeric 0-1}>
#' @param patchmax_verbose logical. Provides additional information about the patchmax project building procedure
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
    proj_id_field = "proj_id",
    proj_fixed_target = TRUE,
    proj_target_field = NULL,
    proj_target_value = NULL,
    proj_target_min_value = -Inf,
    # annual targets
    planning_years = 1,
    annual_target_field = NULL,
    annual_target_value = Inf,
    # scenario variables
    scenario_name = NULL,
    scenario_priorities = NULL,
    scenario_weighting_values = c(1, 1, 1), 
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
    fire_year_field = NULL,
    fire_random_projects = FALSE,
    fire_dynamic_forsys = FALSE,
    # patchmax arguments
    patchmax_proj_number = 1,
    patchmax_proj_size = Inf,
    patchmax_proj_size_min = -Inf,
    patchmax_sample_frac = 0.1,
    patchmax_sample_seed = NULL,
    patchmax_SDW = 0.5,
    patchmax_EPW = 0.5,
    patchmax_exclusion_limit = 1,
    patchmax_verbose = FALSE
    ) {
  
    # 0. SETUP -----------------------------------------------------------------
  
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
      params <- ls()[grepl('stand_data|fire_intersect_table|params', ls()) == FALSE] %>%
        sapply(function(x){tryCatch(get(x), error = function(e) return(0))})
      params <- params[-which(unlist(lapply(params, is.null)))]
      writeLines(jsonlite::toJSON(params, pretty = TRUE), 
                 paste0(relative_output_path, '/', scenario_name, '.json'))
    }

    # 1. PREP DATA -------------------------------------------------------------

    # Load stand data 
    stands <- load_stand_data(stand_data, stand_data_filename, stand_id_field)
  
    # Load fire data (if provided)
    if (run_with_fire) {
      fires <- load_fire_data(fire_intersect_table, fire_intersect_table_filename, stand_id_field, fire_year_field)
    }

    # filter stands by availability
    stands <- stands %>% filter_stands(filter_txt = global_threshold)

    # set up weighting scenarios
    weights <- weight_priorities(
      numPriorities = length(scenario_priorities), 
      weights = scenario_weighting_values)
    
    # reserve key output data
    stands_out <- NULL
    projects_out <- NULL
    subset_out <- NULL

    # 2. LOOP THROUGH WEIGHTS --------------------------------------------------

    for (w in 1:nrow(weights)) { # START WEIGHT LOOP

      ## create the weighted priorities.
      message(paste0("Weighting scenario ", w, " of ", nrow(weights), ": ", paste0(weights[w,], collapse = '-')))

      # calculate objective by weighting priorities
      stands <- stands %>%
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
      
      # 2. SELECT STANDS -------------------------------------------------------

      for (y in 1:planning_years) { # BEGIN YEAR LOOP

        # print planning year
        message(paste('Starting year', y,'----------------'))

        # run with predetermined planning areas
        if (!run_with_patchmax) {
          
          patchstat_out <- build_preset_projects(
            stands = stands_available,
            stand_id_field = stand_id_field,
            stand_area_field = stand_area_field,
            stand_threshold = stand_threshold,
            proj_id_field = proj_id_field,
            proj_target_field = proj_target_field,
            proj_fixed_target = proj_fixed_target,
            proj_target_value = proj_target_value,
            proj_target_min_value = proj_target_min_value,
            proj_number = NULL,
            global_ceiling_field = annual_target_field,
            global_ceiling_value = annual_target_value
          )
          
          projects_selected_y <- patchstat_out[[1]]
          stands_selected_y <- patchstat_out[[2]]
        }
        
        # run with dynamic planning areas
        if (run_with_patchmax) {
          
          patchmax_out <- build_dynamic_projects(
            stands = stands_available, 
            stand_id_field = stand_id_field, 
            stand_area_field = stand_area_field, 
            stand_threshold = stand_threshold,
            patchmax_proj_size = patchmax_proj_size, 
            patchmax_proj_size_min = patchmax_proj_size_min,
            patchmax_proj_number = patchmax_proj_number,
            proj_id_field = proj_id_field,
            proj_target_field = proj_target_field, 
            proj_target_value = proj_target_value, 
            proj_target_min_value = proj_target_min_value, 
            global_ceiling_field = annual_target_field,
            global_ceiling_value = annual_target_value,
            patchmax_SDW = patchmax_SDW, 
            patchmax_EPW = patchmax_EPW, 
            patchmax_exclusion_limit = patchmax_exclusion_limit,
            patchmax_sample_frac = patchmax_sample_frac, 
            patchmax_sample_seed = patchmax_sample_seed,
            patchmax_verbose = patchmax_verbose
          )
          
          projects_selected_y <- patchmax_out[[1]]
          stands_selected_y <- patchmax_out[[2]]
        }
        
        # 5. UPDATE AVAILABILITY BASED ON SELECTION ----------------------------
        
        # report yearly treatment
        s_n <- stands_selected_y %>% filter(DoTreat == 1) %>% pull(stand_id_field) %>% n_distinct()
        p_n <- stands_selected_y %>% filter(DoTreat == 1) %>% pull(treatment_rank) %>% n_distinct()
        o_s <- sum(stands_selected_y$weightedPriority)
        message(paste0('Treating ', s_n, ' stands (', round(s_n/nrow(stands) * 100, 2), '% of total) treated; ',
                       p_n, ' projects (total obj: ', round(o_s, 2), ', avg/prj: ', round(o_s/p_n, 2), ')'))
        
        # record year
        stands_selected_y <- stands_selected_y %>% mutate(ETrt_YR = y)
        
        # accumulate stands selected (and treated) in current year
        stands_selected <- bind_rows(stands_selected, stands_selected_y)
        
        # remove stands or project areas that were treated from available stands
        stands_available <- stands_available %>% 
          filter(get(stand_id_field) %in% unique(stands_selected_y[[stand_id_field]]) == FALSE)
        
        # 6. BEGIN ANNUAL FIRES ------------------------------------------------

        if (run_with_fire) { # BEGIN FIRE LOOP (EXPERIMENTAL)
          
          # randomize project order
          if(fire_random_projects){
            projects_selected_y <- shuffle_projects(projects_selected_y)
          }
          
          # record stands that burned this year
          stands_burned_y <- fires %>% filter(get(fire_year_field) == y)
          
          # report yearly fire
          b_n <- stands_burned_y %>% pull(stand_id_field) %>% n_distinct()
          message(paste0('Burning ', b_n, ' (', round(b_n/nrow(stands) * 100, 2), '%) stands in year ', y))

          # accumulate stands burned during year
          stands_burned <- bind_rows(stands_burned, stands_burned_y)
          
          # remove burnt stands from future selection only if fire_dynamic_forsys is TRUE
          if (fire_dynamic_forsys) {
            stands_available <- stands_available %>% 
              filter(get(stand_id_field) %in% pull(stands_burned, get(stand_id_field)) == FALSE)
          }

        } # END FIRE LOOP (EXPERIMENTAL)

      } # END YEAR LOOP

      # 8. SUMMARIZE DATA ------------------------------------------------------
      
      # tag stands with specific scenario attributes
      stands_out_w <- stands_selected %>%
        select(!!stand_id_field, !!proj_id_field, DoTreat, selected, contains('ETrt_YR'))

      # record fire information if provided
      if (run_with_fire) {
        stands_out_w <- stands_out_w %>% 
          left_join(fires %>% select(!!stand_id_field, Fire_YR = !!fire_year_field), by=stand_id_field)
      }

      # append area to output fields if available
      scenario_output_fields <- unique(c(stand_area_field, scenario_output_fields, 'weightedPriority'))
      
      # tag weighting scenario
      priority_write_tags <- as.data.frame(weights[w,]) %>%
        setNames(paste0('Pr_', 1:length(scenario_priorities), '_', scenario_priorities))
      
      # add scenario output fields to stand output
      join_y <- stands %>% select(!!stand_id_field, any_of(scenario_output_fields))
      stands_out_w <- stands_out_w %>% 
        left_join(join_y, by = stand_id_field) %>%
        bind_cols(priority_write_tags) %>%
        bind_cols(scenario_write_tags := scenario_write_tags)

      # summarize project by subset
      summary_dat <- summarize_projects(
        selected_stands = stands_out_w,
        stands_data = stands,
        stand_id_field = stand_id_field,
        stand_area_field = stand_area_field,
        proj_id_field = proj_id_field,
        scenario_output_grouping_fields = scenario_output_grouping_fields,
        scenario_output_fields = scenario_output_fields)
      
      projects_out_w <- summary_dat$projects %>%
        bind_cols(priority_write_tags) %>%
        bind_cols(scenario_write_tags := scenario_write_tags)
      
      subset_out_w <- summary_dat$subset %>%
        bind_cols(priority_write_tags) %>%
        bind_cols(scenario_write_tags := scenario_write_tags)
      
      stands_out <- bind_rows(stands_out, stands_out_w)
      projects_out <- bind_rows(projects_out, projects_out_w)
      subset_out <- bind_rows(subset_out, subset_out_w)
      
    }  # END WEIGHT LOOP
    
    # 6. WRITE DATA ------------------------------------------------------------
    
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
