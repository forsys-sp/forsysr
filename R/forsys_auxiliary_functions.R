#' Calculate spm for specified fields
#'
#' Used to normalize stands attributes by dividing each field by its maximum
#' value and multiplying by 100. Values are first divided by area if the
#' `area_field` is provides.
#'
#' @param stands stand data
#' @param fields vector of character field names to calculate spm values
#' @param availability_txt Boolean statement describing stand availability
#'
#' @details Unavailable stands are given a value of zero.
#' 
#' @importFrom dplyr pull mutate
#' @export
#' 
calculate_spm <- function(stands, fields=NULL, area_field=NULL, availability_txt=NULL) {
  
  # filter for availability
  include = TRUE
  if (!is.null(availability_txt)) {
    eval_txt <- paste0(
      "stands %>% mutate(out = ifelse(", 
      availability_txt,
      ", TRUE, FALSE)) %>% pull(out)")
    include = eval(parse(text = eval_txt))
  }
  
  # default to calculating spm for all numeric fields if fields is null 
  if (is.null(fields)) {
    x <- stands %>% lapply(is.numeric) %>% unlist()
    fields <- names(x)[x == TRUE]
  }
  
  for (f in fields) {
    values <- pull(stands, f)
    values[include == FALSE] <- 0
    maximum <- max(values, na.rm=T)
    spm_values <- (100 * values / maximum)
    cn <- paste0(f, "_SPM")
    stands <- stands %>% mutate(!!cn := spm_values)
  }
  
  return(stands)
}

#' Calculate pcp for specified fields
#'
#' @param stands stand data
#' @param fields vector of character field names to calculate spm values
#' @param availability_txt Boolean statement describing stand availability
#' 
#' @details Unavailable stands are given a value of zero.
#' 
#' @importFrom dplyr pull mutate
#' @export
#'
calculate_pcp <- function(stands, fields=NULL, availability_txt=NULL){
  
  # filter for availability
  include = TRUE
  if (!is.null(availability_txt)) {
    eval_txt <- paste0("stands %>% mutate(out = ifelse(", availability_txt,", TRUE, FALSE)) %>% pull(out)")
    include = eval(parse(text = eval_txt))
  }
  
  # default to calculating spm for all numeric fields if fields is null 
  if (is.null(fields)) {
    x <- stands %>% lapply(is.numeric) %>% unlist()
    fields <- names(x)[x == TRUE]
  }
  
  for (f in fields) {
    # calculate percent of total and multiple by 100
    cn <- paste0(f, "_PCP")
    values <- as.numeric(pull(stands, f))
    values[include == FALSE] <- 0
    sum.total <- sum(values, na.rm=T)
    stands <- stands %>% mutate(!!cn := (100 * values / sum.total))
  }
  
  return(stands)
}

load_R_config <- function(config_file){
  source(config_file, local = TRUE)
  for (i in ls()) {
    exists(i, envir = parent.frame)
  }
}
#' Load stored in JSON config into the current enviroment
#'
#' @param json_filename character string of forsys json config
#'
load_json_config <- function(json_filename) {
  json_data = readLines(json_filename) %>%
    jsonlite::fromJSON()
  for (i in names(json_data)) {
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
) {
  
  absolute_output_path <- file.path(getwd(), relative_output_path)
  message(paste0("Writing data to ", absolute_output_path))
  
  # Check if output directory exists
  if (!dir.exists(absolute_output_path)) {
    if (run_with_shiny) {
      # ???
    } else {
    }
    dir.create(absolute_output_path, recursive=TRUE)
  } else {
    if (run_with_shiny) {
      list.files(absolute_output_path, full.names = T) %>% file.remove()
    } else {
      if (overwrite_output) {
        list.files(absolute_output_path, full.names = T) %>% file.remove()
      }
    }
  }
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


