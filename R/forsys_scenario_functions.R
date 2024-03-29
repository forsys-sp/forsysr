#' Write a json config file with the user's selected parameters
#'
#' @param config_file Relative path to a config file that defines needed
#' parameters
#' @param scenario_name A name for this scenario
#' @param stand_data_filename Path to the input dataset
#' @param stand_area_field Field for spatial area of the data
#' @param shape_file The path to the saved shapefile
#' @param stand_id_field The field in the stand_data_filename which is a
#' unique ID for each stand
#' @param stand_pcp_spm PCP and SPM values will be calculated for these
#' variables. This should include the priorities and any value outputs.
#' @param use_global_threshold TODO
#' @param global_threshold The land base is the area that is used to calculate
#' the PCP and SPM values. It is currently a single, binary variable that must
#' be computed prior to running the ForSysR script. A blank field means all
#' lands are included in the calculation.
#' @param scenario_priorities Priorities are named here. If only one priority
#' exists, only a weight of one will be used.
#' @param proj_id_field The field in the stand_data_filename that indicates which
#' project or planning area a stand belongs to
#' @param use_stand_threshold TODO
#' @param stand_threshold TODO
#' @param proj_fixed_target TODO
#' @param proj_target_field TODO
#' @param proj_target_value TODO
#' @param scenario_weighting_values Defines the weights and integer steps
#' between weights. The values are for min, max, and step.
#' @param scenario_output_fields This should include the desired fields for the
#' planning area treatment files. Planning area id, priority weights and
#' treatment rank are added automatically.
#' @param scenario_output_grouping_fields Include the smaller and larger groups
#' here for grouping of treated stands.
#' @param overwrite_output Overwrite any existing output of the same name?
#' @param run_with_shiny Sets some output business for better shiny interaction
#' @param fire_intersect_table TOTO
#' @param fire_planning_years = TODO
#' @param fire_annual_target_field TODO
#' @param fire_annual_target TODO
#' @param fire_dynamic_forsys logical. Prevent burnt stands from being selected
#' if TRUE
#' @param fire_random_projects logical. Randomly shuffle project prioritization
#' if TRUE
#' @param scenario_write_tags TODO
#' @export
write_save_file <- function(
    config_file = "",
    scenario_name = "",
    stand_data_filename = "",
    stand_area_field = "",
    shape_file = "",
    stand_id_field = "",
    stand_pcp_spm = NULL,
    use_global_threshold = FALSE,
    global_threshold = NULL,
    scenario_priorities = NULL,
    proj_id_field = "",
    use_stand_threshold = FALSE,
    stand_threshold = NULL,
    proj_fixed_target = FALSE,
    proj_target_field = "",
    proj_target_value = NULL,
    scenario_weighting_values = NULL,
    scenario_output_fields = NULL,
    scenario_output_grouping_fields = NULL,
    overwrite_output = TRUE,
    run_with_shiny = FALSE,
    fire_intersect_table = NULL,
    fire_planning_years = 1,
    fire_annual_target_field = NULL,
    fire_annual_target = NA,
    fire_dynamic_forsys = FALSE,
    fire_random_projects = FALSE,
    scenario_write_tags = NULL
  ) {

  vector_names <- c(
    "config_file",
    "scenario_name",
    "stand_data_filename",
    "stand_area_field",
    "shape_file",
    "stand_id_field",
    "stand_pcp_spm",
    "use_global_threshold",
    "global_threshold",
    "scenario_priorities",
    "proj_id_field",
    "use_stand_threshold",
    "stand_threshold",
    "proj_fixed_target",
    "proj_target_field",
    "proj_target_value",
    "scenario_weighting_values",
    "scenario_output_fields",
    "scenario_output_grouping_fields",
    "overwrite_output",
    "run_with_shiny",
    "fire_intersect_table",
    "fire_planning_years",
    "fire_annual_target_field",
    "fire_annual_target",
    "fire_dynamic_forsys",
    "fire_random_projects",
    "scenario_write_tags"
    )

  vector_data <- vector(mode = "list", length = length(vector_names))
  names(vector_data) <- vector_names

  vector_data$config_file <- config_file
  vector_data$scenario_name <- scenario_name
  vector_data$stand_data_filename <- stand_data_filename
  vector_data$stand_area_field <- stand_area_field
  vector_data$shape_file <- shape_file
  vector_data$stand_id_field <- stand_id_field
  vector_data$stand_pcp_spm <- stand_pcp_spm
  vector_data$use_global_threshold <- use_global_threshold
  vector_data$global_threshold <- global_threshold
  vector_data$scenario_priorities <- scenario_priorities
  vector_data$proj_id_field <- proj_id_field
  vector_data$use_stand_threshold <- use_stand_threshold
  vector_data$stand_threshold <- stand_threshold
  vector_data$proj_fixed_target <- proj_fixed_target
  vector_data$proj_target_field <- proj_target_field
  vector_data$proj_target_value <- proj_target_value
  vector_data$scenario_weighting_values <- scenario_weighting_values
  vector_data$scenario_output_fields <- scenario_output_fields
  vector_data$scenario_output_grouping_fields <- scenario_output_grouping_fields
  vector_data$overwrite_output <- overwrite_output
  vector_data$run_with_shiny <- run_with_shiny
  vector_data$fire_intersect_table <- fire_intersect_table
  vector_data$fire_planning_years <- fire_planning_years
  vector_data$fire_annual_target_field <- fire_annual_target_field
  vector_data$fire_annual_target <- fire_annual_target
  vector_data$fire_dynamic_forsys <- fire_dynamic_forsys
  vector_data$fire_random_projects <- fire_random_projects
  vector_data$scenario_write_tags <- scenario_write_tags

  json_data <- jsonlite::toJSON(vector_data, pretty = TRUE)
  print(json_data)

  output_file_name <- paste0("configs/", scenario_name, ".json")

  if (!dir.exists(file.path(getwd(), "configs"))) {
    print(paste0("Making output directory: ", file.path(getwd(), "configs")))
    dir.create(file.path(getwd(), "configs"))
  }

  writeLines(json_data, output_file_name)

  return(vector_data)
}

#' Take the input object from a shiny server function all at once and then call the write_save_file function
#' This function helps keep the server code clean.
#'
#' @param input input object from a shiny server function
#' @param r_data shiny server data object
#' @return A serialized vector of input choices in json format
#' @export
#'
write_save_file_helper <- function(input, r_data) {
  weight_values <- forsys::weight_values_to_string(input$weight_min, input$weight_max, input$weight_step)

  nesting <- FALSE
  nesting_group_by <- NULL
  nesting_target <- NULL
  nesting_unit <- NULL
  au_target_multiplier <- 1.0

  if (input$use_global_threshold == "Yes") {
    global_threshold <- r_data$global_threshold
  } else {
    global_threshold <- NULL
  }

  if (input$use_stand_threshold == "Yes") {
    stand_threshold <- r_data$stand_threshold
  } else {
    stand_threshold <- NULL
  }

  json <- write_save_file(
    scenario_name = input$scenario_name,
    stand_data_filename = r_data$data_path,
    stand_area_field = input$stand_area_field,
    shape_file = r_data$shape_file,
    stand_id_field = input$stand_id_field,
    stand_pcp_spm = input$priorities_fields,
    use_global_threshold = input$use_global_threshold,
    global_threshold = global_threshold,
    scenario_priorities = input$priorities_fields,
    proj_id_field = input$planning_unit_id_field,
    use_stand_threshold = input$use_stand_threshold,
    stand_threshold = stand_threshold,
    proj_fixed_target = input$proj_fixed_target,
    proj_target_field = input$proj_target_field,
    proj_target_value = input$proj_target_value,
    scenario_weighting_values = weight_values,
    scenario_output_fields = input$outputs_select,
    scenario_output_grouping_fields = input$output_grouping_fields,
    overwrite_output = input$overwrite_output_chk,
    run_with_shiny = TRUE
  )
}

#' Load a json config file written from the write_save_file function.
#'
#' @param filename Relative path and filename to the scenario json
#' @return A vector of input choices
#' @export
#'
read_save_file <- function(filename = "") {

  # TODO check if file exists
  # print(filename)

  json_data <- readLines(filename)
  json_data <- jsonlite::fromJSON(json_data)
}

#' List json files in the configs folder, if it exists.
#'
#' @return A list of found config files
#' @export
#'
list_scenarios <- function() {
  if (dir.exists(file.path(getwd(), "configs"))) {
    output_files <- sapply(list.files("configs"), function(x) paste0("configs/", x))
  }
}


#' Load stored in JSON config into the current enviroment
#'
#' @param json_filename character string of forsys json config

load_json_config <- function(json_filename){
  
  # ignore spurious warning about incomplete final line in json file
  suppressWarnings({
    json_data = readLines(json_filename) %>% jsonlite::fromJSON()
  })
  
  # look for null inputs (saved as an json array of lenght 0)
  is_null <- unlist(
    lapply(json_data, function(x){is.list(x) & length(x) == 0}))
  if(sum(is_null) > 0){
    json_data <- json_data[-which(is_null_input)]
  }
  
  # assign each json item to its respective paramter in the parent environment
  for(i in names(json_data)){
    assign(i, json_data[[i]], envir = parent.frame())
  }
}