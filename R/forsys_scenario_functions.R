#' Write a json config file with the user's selected parameters
#'
write_save_file <- function(...) {
  vector_data <- list(...)
  json_data <- jsonlite::toJSON(vector_data, pretty = TRUE)
  print(json_data)

  output_file_name <- paste0('configs/', vector_data$scenario_name, '.json')
  if (!dir.exists(file.path(getwd(), 'configs'))) {
    print(paste0('Making output directory: ', file.path(getwd(), 'configs')))
    dir.create(file.path(getwd(), "configs"))
  }

  writeLines(json_data, output_file_name)
  return(vector_data)
}

#' Take the input object from a shiny server function all at once and then call the write_save_file function
#' This function helps keep the server code clean.
#'
#' @param input input object from a shiny server function
#' @param data_path System path to save the output file to
#' @return A serialized vector of input choices in json format
#' @export
#'
write_save_file_helper <- function(input, data_path) {

  weight_values <- forsys::weight_values_to_string(input$weight_min, input$weight_max, input$weight_step)

  if (input$use_au) {
    nesting = TRUE
    nesting_group_by = input$au_id_field
    nesting_target = input$au_target_field
    nesting_unit = input$au_unit_field
    au_target_multiplier = input$au_target_multiplier
  } else {
    nesting = FALSE
    nesting_group_by = NULL
    nesting_target = NULL
    nesting_unit = NULL
    au_target_multiplier = 1.0
  }

  json <- write_save_file(
    scenario_name = input$scenario_name,
    scenario_input_standfile = data_path,
    scenario_priorities = input$priorities_fields,
    scenario_weighting_values = weight_values,
    scenario_output_fields = input$outputs_select,
    scenario_output_grouping_fields = input$output_grouping_fields,
    stand_field = input$stand_id_field,
    stand_filter = input$treatment_available_field,
    proj_id = input$planning_unit_id_field,
    proj_target = input$proj_target_field,
    proj_unit = input$proj_unit_field,
    proj_target_multiplier = input$proj_target_multiplier,
    proj_fixed_target = FALSE,
    proj_fixed_area_target = input$proj_fixed_area_target,
    proj_thresholds = input$thresholds_expr,
    overwrite_output = input$overwrite_output_chk
  )
}

#' Load a json config file written from the write_save_file function.
#'
#' @param filename Relative path and filename to the scenario json
#' @return A vector of input choices
#' @export
#'
read_save_file <- function(filename = '') {

	# TODO check if file exists
	# print(filename)

	json_data = readLines(filename)
	json_data = jsonlite::fromJSON(json_data)
}

#' List json files in the configs folder, if it exists.
#'
#' @return A list of found config files
#' @export
#'
list_scenarios <- function() {
	if (dir.exists(file.path(getwd(), 'configs'))) {
		output_files <- sapply(list.files('configs'), function(x) paste0('configs/', x))
	}
}
