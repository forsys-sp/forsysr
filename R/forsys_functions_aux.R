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
#' @return Loaded data.table from the input dataset
#'
#' @importFrom data.table data.table
#'
#' @export
load_dataset <- function(path_to_file) {
  file_type <- stringr::str_sub(path_to_file, start= -3)
  message("Loading Dataset")
  if (file_type == "dbf") {
    standDT <- data.table(foreign::read.dbf(path_to_file))
    message("Read stand file from DBF")
  } else if (file_type == "csv") {
    standDT <- data.table(data.table::fread(path_to_file, header = TRUE))
    message("Read stand file from CSV")
  } else {
    message('Input format not recognized')
  }
  return(standDT)
}

#' Create a dataset by subsetting subunits that were selected in the
#' selectSubunits function and grouping the data by a larger subunit (usually
#' planning areas).
#'
#' @param dt A data table with all the subunits and attributes.
#' @param grouping_vars The variable names by which the data will be grouped.
#' @param summing_vars The variables in the original dataset that need to be
#'   summed over each subunit.
#' @param subset_var TODO
#' @return The selected stands from \code{df}, ordered by \code{priority_SPM},
#'   and selected until the sum of \code{priority_STND} is as close to
#'   \code{treat_target} as possible.
#'  
#' @importFrom dplyr group_by_at vars summarize_at

create_grouped_dataset <- function(dt, grouping_vars, summing_vars, subset_var = NULL) {
  ## Create the grouped data.table by grouping the treated subunits from the previous step.
  if(!is.null(subset_var)){
    dt <- subset(dt[get(subset_var)==1])
  }
  dt <- group_by_at(dt, vars(grouping_vars))
  dt <- data.table::data.table(summarize_at(dt, .vars = vars(summing_vars), .funs = c(sum="sum")))
  names(dt) <- gsub(x = names(dt), pattern = "_sum", replacement = "")
  return(dt)
}

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

#' compile_planning_areas_and_stands
#' 
#' TODO this should go in _results
#'
#' @param stands TODO
#' @param unique_weights TODO
#' @param group_by TODO
#' @param output_fields TODO
#' @return TODO
#'
#' @importFrom dplyr %>% across summarize group_by_at
#' @importFrom data.table setDT
#'
compile_planning_areas_and_stands <- function(
    stands, 
    unique_weights, 
    group_by, 
    output_fields) {
  
  planning_areas <- stands %>% 
    group_by_at(group_by) %>%
    summarize(across(output_fields, sum, .names = "ESum_{.col}"))
  
  return (setDT(planning_areas))
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
  
  