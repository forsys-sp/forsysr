

#' in a config file and pass the name of the file to this run function.
#'
#' @param config_file Relative path to a config file that defines needed parameters
#' @param scenario_name A name for this scenario
#' @param is_dbf Toggle for reading DBF
#' @param is_csv Toggle for reading CSV files
#' @param input_standfile TODO
#' @param stand_field TODO
#' @param pcp_spm PCP and SPM values will be calculated for these variables. This should include the priorities and any value outputs.
#' @param land_base The land base is the area that is used to calculate the PCP and SPM values.
#'                  It is currently a single, binary variable that must be computed prior to running the ForSysR script.
#'                  A blank field means all lands are included in the calculation.
#' @param priorities Priorities are named here. If only one priority exists, only a weight of one will be used.
#' @param stand_group_by TODO
#' @param pa_target TODO
#' @param pa_unit TODO
#' @param pa_target_multiplier TODO
#' @param nesting TODO
#' @param nesting_group_by TODO
#' @param nesting_target TODO
#' @param nesting_target_multiplier TODO
#' @param weighting_values Defines the weights and integer steps between weights. The values are for min, max, and step.
#' @param thresholds Thresholds are defined by type (the first value in the string). The current code only uses one type (Commercial).
#' @param include_stands This defines global threshold values to include stands - i.e. for any threshold type.
#' @param output_fields This should include the desired fields for the planning area treatment files. Planning area id,
#'                      priority weights and treatment rank are added automatically.
#' @param grouping_variables Include the smaller and larger groups here for grouping of treated stands.
#' @param fixed_target Set to have either a fixed area target (TRUE) or a variable area target (FALSE)
#' @param fixed_area_target TODO
#' @param overwrite_output Toggle to overwrite existing output files
#' @return A serialized vector of input choices in json format
#' @export
write_save_file <- function(
  scenario_name = '',
  input_standfile = '',
  write_stand_outputs = '',
  stand_field = 'Cell_ID',
  pcp_spm = c(),
  land_base = '',
  priorities = c(),
  stand_group_by = '',
  pa_target = '',
  pa_unit = '',
  pa_target_multiplier = 0.15,
  nesting = FALSE,
  nesting_group_by = NULL,
  nesting_target = NULL,
  nesting_unit = NULL,
  nesting_target_multiplier = 1.0,
  weighting_values = "0 5 1",
  thresholds = c("Manageable man_alldis == 1") ,
  include_stands = c("man_alldis == 1"),
  output_fields = c("AREA_HA", "TVMBF_STND", "TVMBF_PCP", "HUSUM_STND", "HUSUM_PCP"),
  grouping_variables = c("PA_ID", "Owner"),
  fixed_target = FALSE,
  fixed_area_target = 2000,
  overwrite_output = TRUE
  ) {

	vector_data <- vector(mode='list', length=25)

	names(vector_data) = c(
		'scenario_name',
		'input_standfile',
		'write_stand_outputs',
		'stand_field',
		'pcp_spm',
		'land_base',
		'priorities',
		'stand_group_by',
		'pa_target',
		'pa_unit',
		'pa_target_multiplier',
		'nesting',
		'nesting_group_by',
		'nesting_target',
		'nesting_unit',
		'nesting_target_multiplier',
		'weighting_values',
		'thresholds',
		'include_stands',
		'output_fields',
		'grouping_variables',
		'fixed_target',
		'fixed_area_target',
		'overwrite_output'
		)

	vector_data$scenario_name = scenario_name
	vector_data$input_standfile = input_standfile
	vector_data$write_stand_outputs = write_stand_outputs
	vector_data$stand_field = stand_field
	vector_data$pcp_spm = pcp_spm
	vector_data$land_base = land_base
	vector_data$priorities = priorities
	vector_data$stand_group_by = stand_group_by
	vector_data$pa_target = pa_target
	vector_data$pa_unit = pa_unit
	vector_data$pa_target_multiplier = pa_target_multiplier
	vector_data$nesting = nesting
	vector_data$nesting_group_by = nesting_group_by
	vector_data$nesting_target = nesting_target
	vector_data$nesting_unit = nesting_unit
	vector_data$nesting_target_multiplier = nesting_target_multiplier
	vector_data$weighting_values = weighting_values
	vector_data$thresholds = thresholds
	vector_data$include_stands = include_stands
	vector_data$output_fields = output_fields
	vector_data$grouping_variables = grouping_variables
	vector_data$fixed_target = fixed_target
	vector_data$fixed_area_target = fixed_area_target
	vector_data$overwrite_output = overwrite_output

	json_data <- toJSON(vector_data, pretty = TRUE)

	print(json_data)

	output_file_name <- paste0('configs/', scenario_name, '.json')

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
#' @return A serialized vector of input choices in json format
#' @export
write_save_file_helper <- function(input, data_path) {
	weight_values <- weight_values_to_string(input$weight_min, input$weight_max, input$weight_step)

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
		input_standfile = data_path,
		write_stand_outputs = input$write_stand_outputs_chk,
		stand_field = input$stand_id_field,
		pcp_spm = input$pcp_spm_fields,
		land_base = input$treatment_available_field,
		priorities = input$priorities_fields,
		stand_group_by = input$planning_unit_id_field,
		pa_target = input$pa_target_field,
		pa_unit = input$pa_unit_field,
		pa_target_multiplier = input$pa_target_multiplier,
		nesting = input$use_au,
		nesting_group_by = nesting_group_by,
		nesting_target = nesting_target,
		nesting_unit = nesting_unit,
		nesting_target_multiplier = au_target_multiplier,
		weighting_values = weight_values,
		thresholds = input$thresholds_expr,
		include_stands = c("man_alldis == 1"), # TODO parse include_stands from thresholds, or the other way around
		output_fields = input$outputs_select,
		grouping_variables = input$grouping_fields, # c("PA_ID", "Owner"),
		fixed_target = FALSE,
		fixed_area_target = input$fixed_area_target,
		overwrite_output = input$overwrite_output_chk
		)
}

#' Load a json config file written from the write_save_file function.
#'
#' @param filename Relative path and filename to the scenario json
#' @return A vector of input choices
#' @export
read_save_file <- function(filename = '') {

	# TODO check if file exists
	# print(filename)

	json_data = readLines(filename)
	json_data = fromJSON(json_data)
}

#' List json files in the configs folder, if it exists.
#'
#' @return A list of found config files
#' @export
list_scenarios <- function() {
	if (dir.exists(file.path(getwd(), 'configs'))) {
		output_files <- sapply(list.files('configs'), function(x) glue('configs/{x}'))
	}
}

write_config_file <- function(...) {

}
