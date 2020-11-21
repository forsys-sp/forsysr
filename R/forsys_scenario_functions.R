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
#' @param system_constraint If the constraint is by master nesting unit (i.e. treat the top X planning areas in each
#'                          national forest), set FALSE. If the constraint is by the system (i.e. go to the best planning
#'                          area regardless of where it is located), set TRUE.
#' @param overwrite_output Toggle to overwrite existing output files
#' @return A datatable with the weighted values for the priorities in the \code{priorityList}.
#' @export
write_save_file <- function(
  config_file = '',
  scenario_name = '',
  input_standfile = '',
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
  system_constraint = FALSE,
  overwrite_output = TRUE
  ) {

  
}

read_save_file <- function(...) {

}

write_config_file <- function(...) {

}