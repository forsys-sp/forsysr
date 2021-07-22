#' Write a json config file with the user's selected parameters
#'
#' @param scenario_name A name for this scenario
#' @param num_reps TODO
#' @param input_standfile Path to the input dataset
#' @param write_stand_outputs Whether to write intermediate stand outputs
#' @param stand_field The field in the input_standfile which is a unique ID for each stand
#' @param pcp_spm PCP and SPM values will be calculated for these variables. This should include the priorities and any value outputs.
#' @param land_base The land base is the area that is used to calculate the PCP and SPM values.
#'                  It is currently a single, binary variable that must be computed prior to running the ForSysR script.
#'                  A blank field means all lands are included in the calculation.
#' @param priorities Priorities are named here. If only one priority exists, only a weight of one will be used.
#' @param proj_id The field in the input_standfile that indicates which project or planning area a stand belongs to 
#' @param proj_target TODO
#' @param proj_unit TODO
#' @param proj_target_multiplier TODO
#' @param proj_fixed_target Set to have either a fixed area target (TRUE) or a variable area target (FALSE)
#' @param proj_fixed_area_target If using a fixed target, set the fixed target value here.
#' @param nesting TODO
#' @param nesting_group_by TODO
#' @param nesting_target TODO
#' @param nesting_unit TODO
#' @param nesting_target_multiplier TODO
#' @param weighting_values Defines the weights and integer steps between weights. The values are for min, max, and step.
#' @param thresholds Thresholds are defined by type (the first value in the string). The current code only uses one type (Commercial).
#' @param include_stands This defines global threshold values to include stands - i.e. for any threshold type.
#' @param output_fields This should include the desired fields for the planning area treatment files. Planning area id,
#'                      priority weights and treatment rank are added automatically.
#' @param output_grouping_variables Include the smaller and larger groups here for grouping of treated stands.
#' @param overwrite_output Overwrite any existing output of the same name?
#' @param run_with_shiny Sets some output business for better shiny interaction
#' @param fire_intersect_table TOTO
#' @param fire_planning_years = TODO
#' @param fire_annual_target_field TODO
#' @param fire_annual_target TODO
#' @param fire_dynamic_forsys TODO
#' @param fire_random_projects TODO
#' @param write_tags TODO
#' @return A serialized vector of input choices in json format
#' @export
write_save_file <- function(
	scenario_name = '',
	num_reps = 1,
	input_standfile = '',
	write_stand_outputs = FALSE,
	stand_field = 'CELL_ID',
	pcp_spm = c(),
	land_base = '',
	priorities = c(),
	proj_id = '',
	proj_unit = '',
	proj_target = '',
	proj_target_multiplier = 0.15,
	proj_fixed_target = FALSE,
	proj_fixed_area_target = NULL,
	nesting = FALSE,
	nesting_group_by = NULL,
	nesting_target = NULL,
	nesting_unit = NULL,
	nesting_target_multiplier = 1.0,
	weighting_values = "0 5 1",
	thresholds = c("Manageable man_alldis == 1") ,
	include_stands = c("man_alldis == 1"),
	output_fields = c("AREA_HA", "TVMBF_STND", "TVMBF_PCP", "HUSUM_STND", "HUSUM_PCP"),
	output_grouping_variables = c("PA_ID", "Owner"),
	overwrite_output = TRUE,
	run_with_shiny = FALSE,
	fire_intersect_table = NULL,
	fire_planning_years = 1,
	fire_annual_target_field = NULL,
	fire_annual_target = NA,
	fire_dynamic_forsys = FALSE,
	fire_random_projects = FALSE,
	write_tags = ''
  ) {

	vector_data <- vector(mode='list', length=33)

	names(vector_data) = c(
		'scenario_name',
		'num_reps',
		'input_standfile',
		'write_stand_outputs',
		'stand_field',
		'pcp_spm',
		'land_base',
		'priorities',
		'proj_id',
		'proj_unit',
		'proj_target',
		'proj_target_multiplier',
		'proj_fixed_target',
		'proj_fixed_area_target',
		'nesting',
		'nesting_group_by',
		'nesting_target',
		'nesting_unit',
		'nesting_target_multiplier',
		'weighting_values',
		'thresholds',
		'include_stands',
		'output_fields',
		'output_grouping_variables',
		'overwrite_output',
		'run_with_shiny',
		'fire_intersect_table',
		'fire_planning_years',
		'fire_annual_target_field',
		'fire_annual_target',
		'fire_dynamic_forsys',
		'fire_random_projects',
		'write_tags'
		)

	vector_data$scenario_name = scenario_name
	vector_data$num_reps = num_reps
	vector_data$input_standfile = input_standfile
	vector_data$write_stand_outputs = write_stand_outputs
	vector_data$stand_field = stand_field
	vector_data$pcp_spm = pcp_spm
	vector_data$land_base = land_base
	vector_data$priorities = priorities
	vector_data$proj_id = proj_id
	vector_data$proj_unit = proj_unit
	vector_data$proj_target = proj_target
	vector_data$proj_target_multiplier = proj_target_multiplier
	vector_data$proj_fixed_target = proj_fixed_target
	vector_data$proj_fixed_area_target = proj_fixed_area_target
	vector_data$nesting = nesting
	vector_data$nesting_group_by = nesting_group_by
	vector_data$nesting_target = nesting_target
	vector_data$nesting_unit = nesting_unit
	vector_data$nesting_target_multiplier = nesting_target_multiplier
	vector_data$weighting_values = weighting_values
	vector_data$thresholds = thresholds
	vector_data$include_stands = include_stands
	vector_data$output_fields = output_fields
	vector_data$output_grouping_variables = output_grouping_variables
	vector_data$overwrite_output = overwrite_output
	vector_data$run_with_shiny = run_with_shiny
	vector_data$fire_intersect_table = fire_intersect_table
	vector_data$fire_planning_years = fire_planning_years
	vector_data$fire_annual_target_field = fire_annual_target_field
	vector_data$fire_annual_target = fire_annual_target
	vector_data$fire_dynamic_forsys = fire_dynamic_forsys
	vector_data$fire_random_projects = fire_random_projects
	vector_data$write_tags = write_tags

	json_data <- jsonlite::toJSON(vector_data, pretty = TRUE)

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
#' @param data_path System path to save the output file to
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
		stand_field = input$stand_field,
		pcp_spm = input$pcp_spm_fields,
		land_base = input$treatment_available_field,
		priorities = input$priorities_fields,
		proj_id = input$planning_unit_id_field,
		proj_target = input$proj_target_field,
		proj_unit = input$proj_unit_field,
		proj_target_multiplier = input$proj_target_multiplier,
		proj_fixed_target = FALSE,
		proj_fixed_area_target = input$proj_fixed_area_target,
		nesting = input$use_au,
		nesting_group_by = nesting_group_by,
		nesting_target = nesting_target,
		nesting_unit = nesting_unit,
		nesting_target_multiplier = au_target_multiplier,
		weighting_values = weight_values,
		thresholds = input$thresholds_expr,
		include_stands = c("man_alldis == 1"), # TODO parse include_stands from thresholds, or the other way around
		output_fields = input$outputs_select,
		output_grouping_variables = input$output_grouping_variables, # c("PA_ID", "Owner"),
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
	json_data = jsonlite::fromJSON(json_data)
}

#' List json files in the configs folder, if it exists.
#'
#' @return A list of found config files
#' @export
list_scenarios <- function() {
	if (dir.exists(file.path(getwd(), 'configs'))) {
		output_files <- sapply(list.files('configs'), function(x) paste0('configs/', x))
	}
}

