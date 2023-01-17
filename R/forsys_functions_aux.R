load_R_config <- function(config_file){
  source(config_file, local = TRUE)
  for(i in ls()){
    exists(i, envir = parent.frame)
  }
}
#' Load stored in JSON config into the current enviroment
#'
#' @param json_filename character string of forsys json config
#'
load_json_config <- function(json_filename){
  json_data = readLines(json_filename) %>%
    jsonlite::fromJSON()
  for(i in names(json_data)){
    assign(i, json_data[[i]], env = parent.frame())
  }
}

#' Load the input dataset. Supports both CSV and DBF.
#'
#' @param path_to_file Path to an input dataset
#'
#' @return Stand data
#'
#' @importFrom stringr str_sub
#' @importFrom foreign read.dbf
#' @importFrom data.table data.table
#'
#' @export
load_dataset <- function(path_to_file) {
  file_type <- stringr::str_sub(path_to_file, start= -3)
  message("Loading Dataset")
  if (file_type == "dbf") {
    stand_data <- foreign::read.dbf(path_to_file)
    message("Read stand file from DBF")
  } else if (file_type == "csv") {
    stand_data <- data.table::fread(path_to_file, header = TRUE, data.table = FALSE)
    message("Read stand file from CSV")
  } else if (file_type == "shp") {
    stand_data <- sf::st_read(path_to_file)
    message("Read stand file from SHP")
  } else {
    message('Input format not recognized')
  }
  return(stand_data)
}


#' Create directory saving forsys output data
#'
#' @param relative relative path where new directory is placed
#' @param run_with_shiny ???
#' @param overwrite_output logical. Whether to first delete existing data in directory
#'
#' @return
#' @export

create_output_directory <- function(
    relative_output_path, 
    run_with_shiny, 
    overwrite_output
){
  
  absolute_output_path <- file.path(getwd(), relative_output_path)
  
  # Check if output directory exists
  if (!dir.exists(absolute_output_path)) {
    if (run_with_shiny) {
      # ???
    } else {
      message(paste0("Making output directory: ", absolute_output_path))
    }
    dir.create(absolute_output_path, recursive=TRUE)
  } else {
    message(paste0("Output directory, ", absolute_output_path, ", already exists..."))
    if (run_with_shiny) {
      list.files(absolute_output_path, full.names = T) %>% file.remove()
    } else {
      if (overwrite_output) {
        list.files(absolute_output_path, full.names = T) %>% file.remove()
        message('...Deleting previous files')
      }
    }
  }
}

#' Create a dataset by subsetting subunits that were selected in the
#' selectSubunits function and grouping the data by a larger subunit (usually
#' planning areas).
#'
#' @param data data with all the subunits and attributes.
#' @param grouping_vars The variable names by which the data will be grouped.
#' @param summing_vars The variables in the original dataset that need to be
#'   summed over each subunit.
#' @return The selected stands from \code{df}, ordered by \code{priority_SPM},
#'   and selected until the sum of \code{priority_STND} is as close to
#'   \code{treat_target} as possible.
#'  
#' @importFrom dplyr group_by_at vars summarize_at

create_grouped_dataset <- function(data, grouping_vars, summing_vars) {
  
  grouping_vars <- unique(grouping_vars)
  summing_vars <- unique(summing_vars)
  
  data <- data %>% 
    group_by_at(vars(grouping_vars)) %>%
    summarize_at(vars(summing_vars), sum)
  
  return(data)
}


summarize_projects <- function(
    selected_stands,
    stands_data,
    stand_id_field,
    proj_id_field,
    scenario_output_grouping_fields,
    scenario_output_fields
){
  
  # append specified output attributes to selected stands
  selected_stands <- selected_stands  %>%
    select(stand_id_field, proj_id_field, DoTreat, ETrt_YR) %>%
    left_join(stands_data %>% select(
      !!stand_id_field, 
      any_of(scenario_output_grouping_fields), 
      any_of(scenario_output_fields),
      weightedPriority),
      by = stand_id_field, suffix = c("", ".dup"))
  
  # summarize selected stands by grouping fields and tag with ETrt_ prefix
  projects_etrt_out_w <- selected_stands %>%
    filter(DoTreat == 1) %>%
    create_grouped_dataset(
      grouping_vars = unique(c(proj_id_field, scenario_output_grouping_fields, 'ETrt_YR')),
      summing_vars = c(scenario_output_fields, 'weightedPriority')) %>%
    rename_with(.fn = ~ paste0("ETrt_", .x), .cols = scenario_output_fields)

  # summarize available stands by grouping fields and tag with ESum_ prefix
  projects_esum_out_w <- selected_stands %>%
    create_grouped_dataset(
      grouping_vars = unique(c(proj_id_field, scenario_output_grouping_fields, 'ETrt_YR')),
      summing_vars = c(scenario_output_fields, 'weightedPriority')) %>%
    rename_with(.fn = ~ paste0("ESum_", .x), .cols = scenario_output_fields)
  
  # rank projects
  projects_rank <- projects_etrt_out_w %>%
    group_by(!!proj_id_field := get(proj_id_field)) %>%
    summarize_at(vars(weightedPriority), sum) %>%
    arrange(-weightedPriority) %>%
    mutate(treatment_rank = rank(-weightedPriority)) %>%
    select(!!proj_id_field, treatment_rank)
  
  # join etrt w/ esum outputs
  projects_etrt_esum_out_w <- projects_etrt_out_w %>%
    inner_join(projects_esum_out_w, 
               by=unique(c(proj_id_field, scenario_output_grouping_fields))) %>%
    left_join(projects_rank, by = proj_id_field) %>%
    arrange(ETrt_YR, -weightedPriority) %>%
    replace(is.na(.), 0)
  
  return(projects_etrt_esum_out_w)
  
}

#' Write individual stand output to file
#' TODO this should go in _results
#'
#' @param selected_stands TODO
#' @param dir TODO
#' @param name TODO
#' @param write_fields TODO
#' @return Undefined
#'
#' @importFrom dplyr %>%
#'
write_stand_outputs_to_file <- function(selected_stands, dir, name, write_fields) {
  stand_output_file <- paste0(dir, "/stnd_",  name, ".csv")
  data.table::fwrite(selected_stands %>% dplyr::select(write_fields), stand_output_file)
}


#' weight_values_to_string
#' 
#' Takes discrete minimum, maximum, and step values and turns them into
#' the string that forsys expects
#'
#' @param min Minimum value
#' @param max Maximum value
#' @param step Increment value
#' @return String of step values
#' @export
#' 
weight_values_to_string <- function(min = 0, max = 5, step = 1) {
  paste0(min, ' ', max, ' ', step)
}

#' Inverse of weight_values_to_string
#'
#' @param weight_str A string of minimum, maximum, and step weights
#' @return A list of values cast as integers
#' @export
#' 
weight_string_to_values <- function(weight_str) {
  vals <- stringr::str_split(weight_str, ' ')
  vals <- sapply(vals, as.numeric)
}

#' Print the version information for Forsys.R
#'
#' @return A string representing the version of Forsys.R
#' @export
version <- function() {ver <- paste('forsys version: ', utils::packageVersion('forsys'))}
  

printSpecsDocument <- function(subunit, priorities, timber_threshold, volume_constraint) {
  parameters <- paste0("ForSysR was designed and coded by Cody Evers and Rachel Houtman, run on: ", Sys.Date(), "\n",
                       "This code creates attainment graphs for to explore both WUI exposure (HUIDW) and merchantable timber volume\n",
                       "with the following ForSys settings:\n",
                       "1) Subunit = PA_ID_New\n",
                       "2) treated area = 15% of manageable planning area, field = man_alldis\n",
                       "3) project number = NA\n",
                       "4) prioritize by potential merchantable volume AND WUI exposure\n",
                       "5) Timber threshold > 0\n",
                       "6) No forest volume constraint\n",
                       "7) Running on ForSys\n",
                       "\n",
                       "Required files:\n",
                       "1) ForSys output - proj_all.csv")
  writeLines(parameters, file("README.txt"))
}


