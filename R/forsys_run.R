#' forsys: A package for objective optimization of forest planning
#'
#' ForSys is a multi-objective spatial prioritization system that was designed to explore how 
#' specific investment strategies aimed at improving forest conditions change under a range of 
#' management constraints to achieve specific outcomes and understand tradeoffs. Forsys.app 
#' modernizes how the USFS and our partners identify, prioritize, and implement treatment 
#' activities across the landscape by using cloud-based, geospatial technology to explore 
#' and rapidly visualize various management scenarios and treatment optimization decisions.
#' 
#' @section forsys functions:
#' forsys_run
#'
#' @docType package
#' @name forsys
NULL
#> NULL

#' Run the ForSys treatment planner. Either provide parameters, or define parameters
#' in a config file and pass the name of the file to this run function.
#'
#' @param config_file Relative path to a config file that defines needed parameters
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
#' @param grouping_variables Include the smaller and larger groups here for grouping of treated stands.
#' @param overwrite_output Overwrite any existing output of the same name?
#' @param run_with_shiny Sets some output business for better shiny interaction
#' @param fire_intersect_table TOTO
#' @param fire_planning_years = TODO
#' @param fire_annual_target_field TODO
#' @param fire_annual_target TODO
#' @param fire_dynamic_forsys TODO
#' @param fire_random_projects TODO
#' @param write_tags TODO

#' @return 
#' @export
forsys_run <- function(
	config_file = '',
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
	grouping_variables = c("PA_ID", "Owner"),
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
run(
	config_file = config_file,
    scenario_name = scenario_name,
    num_reps = num_reps,
    input_standfile = input_standfile,
    write_stand_outputs = write_stand_outputs,
    stand_field = stand_field,
    pcp_spm = pcp_spm,
    land_base = land_base,
    priorities = priorities,
    proj_id = proj_id,
    proj_unit = proj_unit,
    proj_target = proj_target,
    proj_target_multiplier = proj_target_multiplier,
    proj_fixed_target = proj_fixed_target,
    proj_fixed_area_target = proj_fixed_area_target,
    nesting = nesting,
    nesting_group_by = nesting_group_by,
    nesting_target = nesting_target,
    nesting_unit = nesting_unit,
    nesting_target_multiplier = nesting_target_multiplier,
    weighting_values = weighting_values,
    thresholds = thresholds,
    include_stands = include_stands,
    output_fields = output_fields,
    grouping_variables = grouping_variables,
    overwrite_output = overwrite_output,
    run_with_shiny = run_with_shiny,
    fire_intersect_table = fire_intersect_table,
    fire_planning_years = fire_planning_years,
    fire_annual_target_field = fire_annual_target_field,
    fire_annual_target = fire_annual_target,
    fire_dynamic_forsys = fire_dynamic_forsys,
    fire_random_projects = fire_random_projects,
    write_tags = write_tags
	)
}

#' Run the ForSys treatment planner. Either provide parameters, or define parameters
#' in a config file and pass the name of the file to this run function.
#'
#' @param data_path Relative path to a config file that defines needed parameters
#' @return A datatable with the weighted values for the priorities in the \code{priorityList}.
#' @export
load_data <- function(data_path) {
	load_dataset(data_path)
}