library(R6)

ForsysScenario <- R6::R6Class("ForsysScenario",
  public = list(
    #' @field config_file Relative path to a config file that defines needed
    #' parameters
    config_file = NULL,
    #' @field scenario_name A name for this scenario
    scenario_name = "",
    #' @field num_reps TODO
    num_reps = 1,
    #' @field scenario_stand_filename Path to the input dataset
    scenario_stand_filename = "",
    #' @field A loaded data.table for forys to run on. Optional, will override
    #' scenario_stand_filename
    shiny_data = "",
    #' @field stand_id_field The field in the scenario_stand_filename which
    #' is a unique ID for each stand
    stand_id_field = "",
    #' @field stand_pcp_spm PCP and SPM values will be calculated for these
    #' variables. This should include the priorities and any value outputs.
    stand_pcp_spm = NULL,
    #' @field stand_filter The land base is the area that is used to calculate
    #' the PCP and SPM values. It is currently a single, binary variable that
    #' must be computed prior to running the ForSysR script. A blank field
    #' means all lands are included in the calculation.
    stand_filter = NULL,
    #' @field scenario_priorities Priorities are named here. If only one
    #' priority exists, only a weight of one will be used.
    scenario_priorities = NULL,
    #' @field proj_id The field in the scenario_stand_filename that indicates
    #' which project or planning area a stand belongs to
    proj_id = "",
    #' @field proj_thresholds TODO
    proj_thresholds = NULL,
    #' @field proj_fixed_target TODO
    proj_fixed_target = FALSE,
    #' @field proj_target_field TODO
    proj_target_field = "",
    #' @field proj_target_value TODO
    proj_target_value = NULL,
    #' @field scenario_weighting_values Defines the weights and integer steps
    #' between weights. The values are for min, max, and step.
    scenario_weighting_values = NULL,
    #' @field scenario_output_fields This should include the desired fields for
    #' the planning area treatment files. Planning area id, priority weights and
    #' treatment rank are added automatically.
    scenario_output_fields = NULL,
    #' @field scenario_output_grouping_fields Include the smaller and larger
    #' groups here for grouping of treated stands.
    scenario_output_grouping_fields = NULL,
    #' @field overwrite_output Overwrite any existing output of the same name?
    overwrite_output = TRUE,
    #' @field run_with_shiny Sets some output business for better shiny
    #' interaction
    run_with_shiny = FALSE,
    #' @field scenario_write_tags TODO
    scenario_write_tags = NULL,

    #' @description
    #' Initialize the ForsysScenario object. Optionally provide an input
    #' object from the forsys.app shiny interface to populate parameters
    #' @param input A shiny input collection from forsys.app
    #' @return New ForsysScenario object
    initialize = function(input = NULL, json_file = NULL) {
      if (!is.null(input) && !is.null(json_file)) {
        message('Cannot instantiate object from both shiny input and a json file.')
        message('Provide one or the other.')
        return(NULL)
      }

      if (!is.null(input)) {
        weight_values <- forsys::weight_values_to_string(input$weight_min,
                                                        input$weight_max,
                                                        input$weight_step)

        # if (input$use_au) {
        #   nesting <- TRUE
        #   nesting_group_by <- input$au_id_field
        #   nesting_target <- input$au_target_field
        #   nesting_unit <- input$au_unit_field
        #   au_target_multiplier <- input$au_target_multiplier
        # } else {
        #   nesting <- FALSE
        #   nesting_group_by <- NULL
        #   nesting_target <- NULL
        #   nesting_unit <- NULL
        #   au_target_multiplier <- 1.0
        # }

        nesting <- FALSE
        nesting_group_by <- NULL
        nesting_target <- NULL
        nesting_unit <- NULL
        au_target_multiplier <- 1.0

        stand_filter <- NULL
        project_filter <- parse_thresholds(input$proj_threshold_field, input$proj_threshold_op, input$proj_threshold_value)

        self$scenario_name <- input$scenario_name
        self$scenario_stand_filename <- data_path
        self$scenario_priorities <- input$priorities_fields
        self$scenario_weighting_values <- weight_values
        self$scenario_output_fields <- input$outputs_select
        self$scenario_output_grouping_fields <- input$output_grouping_fields
        self$stand_id_field <- input$stand_id_field
        self$stand_pcp_spm <- input$priorities_fields
        self$stand_filter <- stand_filter
        self$use_global_threshold <- input$use_global_threshold
        self$global_threshold_field <- input$global_threshold_field
        self$global_threshold_op <- input$global_threshold_op
        self$global_threshold_value <- input$global_threshold_value
        self$proj_threshold_field <- input$proj_threshold_field
        self$proj_threshold_op <- input$proj_threshold_op
        self$proj_threshold_value <- input$proj_threshold_value
        self$proj_id <- input$planning_unit_id_field
        self$proj_thresholds <- input$project_filter
        self$proj_fixed_target <- input$proj_fixed_target
        self$proj_target_field <- input$proj_target_field
        self$proj_target_value <- input$proj_target_value
        # proj_fixed_target_value <- input$proj_fixed_target_value,
        # proj_variable_target_multiplier <- input$proj_variable_target_multiplier,
        self$overwrite_output <- input$overwrite_output_chk
        self$run_with_shiny <- TRUE

      }

      if (!is.null(json_file) {
        json_data <- readLines(filename)
        json_data <- jsonlite::fromJSON(json_data)
      }

      self$load_message()
    },

    load_message = function() {
      cat(paste0("ForsysScenario class created"))
    },

    #' @description
    #' Write a json config file with the user's selected parameters
    #' @return TODO
    write_save_file = function() {
      vector_names <- c(
        "config_file",
        "scenario_name",
        "scenario_stand_filename",
        "stand_id_field",
        "stand_pcp_spm",
        "stand_filter",
        "scenario_priorities",
        "use_global_threshold",
        "global_threshold_field",
        "global_threshold_op",
        "global_threshold_value",
        "proj_threshold_field",
        "proj_threshold_op",
        "proj_threshold_value",
        "proj_id",
        "proj_thresholds",
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

      vector_data$config_file <- self$config_file
      vector_data$scenario_name <- self$scenario_name
      vector_data$scenario_stand_filename <- self$scenario_stand_filename
      vector_data$stand_id_field <- self$stand_id_field
      vector_data$stand_pcp_spm <- self$stand_pcp_spm
      vector_data$stand_filter <- self$stand_filter
      vector_data$scenario_priorities <- self$scenario_priorities
      vector_data$use_global_threshold <- self$use_global_threshold
      vector_data$global_threshold_field <- self$global_threshold_field
      vector_data$global_threshold_op <- self$global_threshold_op
      vector_data$global_threshold_value <- self$global_threshold_value
      vector_data$proj_threshold_field <- self$proj_threshold_field
      vector_data$proj_threshold_op <- self$proj_threshold_op
      vector_data$proj_threshold_value <- self$proj_threshold_value
      vector_data$proj_id <- self$proj_id
      vector_data$proj_thresholds <- self$proj_thresholds
      vector_data$proj_fixed_target <- self$proj_fixed_target
      vector_data$proj_target_field <- self$proj_target_field
      vector_data$proj_target_value <- self$proj_target_value
      vector_data$scenario_weighting_values <- self$scenario_weighting_values
      vector_data$scenario_output_fields <- self$scenario_output_fields
      vector_data$scenario_output_grouping_fields <- self$scenario_output_grouping_fields
      vector_data$overwrite_output <- self$overwrite_output
      vector_data$run_with_shiny <- self$run_with_shiny
      vector_data$scenario_write_tags <- scenario_write_tags

      json_data <- jsonlite::toJSON(vector_data, pretty = TRUE)
      # print(json_data)

      output_file_name <- paste0("configs/", scenario_name, ".json")

      if (!dir.exists(file.path(getwd(), "configs"))) {
        print(
          paste0(
            "Making output directory: ",
            file.path(getwd(), "configs")
          )
        )
        dir.create(file.path(getwd(), "configs"))
      }

      writeLines(json_data, output_file_name)

      return(vector_data)
    },

    #' @description
    #' Load a json config file written from the write_save_file function.
    #'
    #' @param filename Relative path and filename to the scenario json
    #' @return A vector of input choices
    read_save_file = function(filename = "") {
      json_data <- readLines(filename)
      json_data <- jsonlite::fromJSON(json_data)
    }
  ) # end public list

)

FireScenario <- R6::R6Class("FireScenario",
  inherit = ForsysScenario,
  public = list(
    fire_intersect_table = NULL,
    #' @field fire_intersect_table TOTO
    fire_planning_years = NULL,
    #' @field fire_planning_years = TODO
    fire_annual_target_field = NULL,
    #' @field fire_annual_target_field TODO
    fire_annual_target = NULL,
    #' @field fire_annual_target TODO
    fire_dynamic_forsys = NULL,
    #' @field fire_dynamic_forsys logical. Prevent burnt stands from being
    #' selected if TRUE
    fire_random_projects = NULL
    #' @field fire_random_projects logical. Randomly shuffle project
    #' prioritization if TRUE
  )

)



#' Write a json config file with the user's selected parameters
#'
#' @param config_file Relative path to a config file that defines needed
#' parameters
#' @param scenario_name A name for this scenario
#' @param scenario_stand_filename Path to the input dataset
#' @param stand_id_field The field in the scenario_stand_filename which is a
#' unique ID for each stand
#' @param stand_pcp_spm PCP and SPM values will be calculated for these
#' variables. This should include the priorities and any value outputs.
#' @param global_threshold_field The land base is the area that is used to
#' calculate the PCP and SPM values. It is currently a single, binary variable
#' that must be computed prior to running the ForSysR script. A blank field
#' means all lands are included in the calculation.
#' @param scenario_priorities Priorities are named here. If only one priority
#' exists, only a weight of one will be used.
#' @param proj_id The field in the scenario_stand_filename that indicates which
#' project or planning area a stand belongs to
#' @param proj_thresholds TODO
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
#' @param fire_dynamic_forsys TODO
#' @param fire_random_projects TODO
#' @param scenario_write_tags TODO
#' @export
#'
write_save_file <- function(
    config_file = "",
    scenario_name = "",
    scenario_stand_filename = "",
    stand_id_field = "",
    stand_pcp_spm = NULL,
    stand_filter = "",
    use_global_threshold = NULL,
    global_threshold_field = NULL,
    global_threshold_op = NULL,
    global_threshold_value = NULL,
    proj_threshold_field = NULL,
    proj_threshold_op = NULL,
    proj_threshold_value = NULL,
    scenario_priorities = NULL,
    proj_id = "",
    proj_thresholds = NULL,
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

  params <- as.list(match.call())
  print(names(argg))

  vector_names <- c(
    "config_file",
    "scenario_name",
    "scenario_stand_filename",
    "stand_id_field",
    "stand_pcp_spm",
    "stand_filter",
    "scenario_priorities",
    "use_global_threshold",
    "global_threshold_field",
    "global_threshold_op",
    "global_threshold_value",
    "proj_threshold_field",
    "proj_threshold_op",
    "proj_threshold_value",
    "proj_id",
    "proj_thresholds",
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
  vector_data$scenario_stand_filename <- scenario_stand_filename
  vector_data$stand_id_field <- stand_id_field
  vector_data$stand_pcp_spm <- stand_pcp_spm
  vector_data$stand_filter <- stand_filter
  vector_data$scenario_priorities <- scenario_priorities
  vector_data$use_global_threshold <- use_global_threshold
  vector_data$global_threshold_field <- global_threshold_field
  vector_data$global_threshold_op <- global_threshold_op
  vector_data$global_threshold_value <- global_threshold_value
  vector_data$proj_threshold_field <- proj_threshold_field
  vector_data$proj_threshold_op <- proj_threshold_op
  vector_data$proj_threshold_value <- proj_threshold_value
  vector_data$proj_id <- proj_id
  vector_data$proj_thresholds <- proj_thresholds
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
  # print(json_data)

  output_file_name <- paste0("configs/", scenario_name, ".json")

  if (!dir.exists(file.path(getwd(), "configs"))) {
    print(paste0("Making output directory: ", file.path(getwd(), "configs")))
    dir.create(file.path(getwd(), "configs"))
  }

  writeLines(json_data, output_file_name)

  return(vector_data)

}

#' Take the input object from a shiny server function all at once and then call
#' the write_save_file function This function helps keep the server code clean.
#'
#' @param input input object from a shiny server function
#' @param data_path System path to save the output file to
#' @return A serialized vector of input choices in json format
#' @export
#'
write_save_file_helper <- function(input, data_path) {

  weight_values <- forsys::weight_values_to_string(input$weight_min, input$weight_max, input$weight_step)

  # if (input$use_au) {
  #   nesting <- TRUE
  #   nesting_group_by <- input$au_id_field
  #   nesting_target <- input$au_target_field
  #   nesting_unit <- input$au_unit_field
  #   au_target_multiplier <- input$au_target_multiplier
  # } else {
  #   nesting <- FALSE
  #   nesting_group_by <- NULL
  #   nesting_target <- NULL
  #   nesting_unit <- NULL
  #   au_target_multiplier <- 1.0
  # }

  nesting <- FALSE
  nesting_group_by <- NULL
  nesting_target <- NULL
  nesting_unit <- NULL
  au_target_multiplier <- 1.0

  stand_filter <- NULL
  project_filter <- parse_thresholds(input$proj_threshold_field, input$proj_threshold_op, input$proj_threshold_value)

  # WIP: alternative specification for writing scenairo configo to JSON
  # vector_data <- NULL
  # for(i in 1:length(names(input))){
  #   print(i)
  #   nm <- names(input)[i]
  #   val <- input[[nm]]
  #   vector_data[[i]] <- ifelse(is.null(val), NULL, val)
  #   names(vector_data)[[i]] <- nm
  # }
  # json_data <- jsonlite::toJSON(vector_data, pretty = TRUE)

  json <- write_save_file(
    scenario_name = input$scenario_name,
    scenario_stand_filename = data_path,
    scenario_priorities = input$priorities_fields,
    scenario_weighting_values = weight_values,
    scenario_output_fields = input$outputs_select,
    scenario_output_grouping_fields = input$output_grouping_fields,
    stand_id_field = input$stand_id_field,
    stand_pcp_spm = input$priorities_fields,
    stand_filter = stand_filter,
    use_global_threshold = input$use_global_threshold,
    global_threshold_field = input$global_threshold_field,
    global_threshold_op = input$global_threshold_op,
    global_threshold_value = input$global_threshold_value,
    proj_threshold_field = input$proj_threshold_field,
    proj_threshold_op = input$proj_threshold_op,
    proj_threshold_value = input$proj_threshold_value,
    proj_id = input$planning_unit_id_field,
    proj_thresholds = input$project_filter,
    proj_fixed_target = input$proj_fixed_target,
    proj_target_field = input$proj_target_field,
    proj_target_value = input$proj_target_value,
    # proj_fixed_target_value = input$proj_fixed_target_value,
    # proj_variable_target_multiplier = input$proj_variable_target_multiplier,
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

write_config_file <- function(...) {

}


#' Load stored in JSON config into the current enviroment
#'
#' @param json_filename character string of forsys json config

load_json_config <- function(json_filename){
  json_data <- readLines(json_filename) %>%
    jsonlite::fromJSON()
  for (i in names(json_data)) {
    assign(i, json_data[[i]], env = parent.frame())
  }
}
