options(shiny.maxRequestSize = 2000*1024^2)


validate_inputs <- function() {

}

server <- function(input, output, session) {

	# TODO These tooltips don't work
	addTooltip(session, id = 'save_scenario_button', title = 'Save Scenario')
	addTooltip(session, id = 'save_and_run_but', title = 'Save and Run Scenario')
	addTooltip(session, id = 'clear_form_but', title = 'Clear Form')

	available_scenarios <- list_scenarios()
	updateSelectInput(session, 'select_scenario', choices = available_scenarios)

	if (length(available_scenarios) == 0) {
		updateTabsetPanel(session, 'main_nav', selected = 'scenario_setup_panel')
	}

	r_data <- reactiveValues(data = NULL, data_path = NULL, ext = NULL, json = '')
	r_choices <- reactiveValues(priorities = NULL, outputs = NULL)

	data <- reactive({
		# Block until file is completely uploaded
		req(input$file_select)

		# fileInput returns an object with parameters
		file <- input$file_select
		ext <- tools::file_ext(file$datapath)

		perm_data_path <- glue('data/', file$name)
		file.copy(input$file_select$datapath, perm_data_path)

		r_data$data_path <- perm_data_path
		r_data$ext <- ext
		
		# Remember R returns the last thing assigned to, no return needed
		r_data$data <- switch(ext, 
			csv = load_dataset(perm_data_path, FALSE), 
			dbf = load_dataset(perm_data_path, TRUE))
		})

	output$input_data <- renderDataTable({
		head(r_data$data, 10)
		})

	observeEvent(data(), {
		data()
		})

	observeEvent(r_data$data, {
		choices <- colnames(r_data$data)
		updateSelectInput(session, 'stand_field', choices = choices)
		updateSelectInput(session, 'pcp_spm_fields', choices = choices)
		updateSelectInput(session, 'land_base_field', choices = choices)
		updateSelectInput(session, 'stand_group_by_field', choices = choices)
		# updateSelectInput(session, 'pa_target_field', choices = choices)
		updateSelectInput(session, 'pa_unit_field', choices = choices)
		updateSelectInput(session, 'pa_target_multiplier_field', choices = choices)
		updateSelectInput(session, 'grouping_fields', choices = choices)

		# print(r_data$json)

		# TODO implement better check for json data
		if (length(r_data$json) > 1) {
			updateTextInput(session, 'scenario_name', value = r_data$json$scenario_name)
			updateCheckboxInput(session, 'write_stand_outputs_chk', value = r_data$json$write_stand_outputs)
			updateSelectInput(session, 'stand_field', selected = r_data$json$stand_field)
			updateSelectInput(session, 'pcp_spm_fields', selected = r_data$json$pcp_spm)

			############################
			# Populate outputs now that we have some options
			select <- c(reactive_output_field(), reactive_pcp_spm_fields(), reactive_pa_unit_field()) # In case a previous selection was made, get them
			updateSelectInput(session, 'outputs_select', choices = choices, selected = select)

			# Now create the spm_pcp (priorities) choices (these aren't fields from the input data)
			choices <- get_spm_pcp_names(reactive_pcp_spm_fields())
			updateSelectInput(session, 'priorities_fields', choices = choices, selected = r_data$json$priorities)
			############################


			updateSelectInput(session, 'land_base_field', selected = r_data$json$land_base)
			updateSelectInput(session, 'stand_group_by_field', selected = r_data$json$stand_group_by)
			updateTextInput(session, 'pa_target_field', value = r_data$json$pa_target)
			updateSelectInput(session, 'pa_unit_field', selected = r_data$json$pa_unit)
			updateNumericInput(session, 'pa_target_multiplier_field', value = r_data$json$pa_target_multiplier)
			
			# updateTextInput(session, 'nesting', value = r_data$json$nesting)
			# updateTextInput(session, 'nesting_group_by', value = r_data$json$nesting_group_by)
			# updateTextInput(session, 'nesting_target', value = r_data$json$nesting_target)
			# updateTextInput(session, 'nesting_unit', value = r_data$json$nesting_unit)
			# updateTextInput(session, 'nesting_target_multiplier', value = r_data$json$nesting_target_multiplier)

			# weighting_values <- weight_string_to_values(r_data$json$weighting_values)
			# updateNumericInput(session, 'weight_min', value = weighting_values[1])
			# updateNumericInput(session, 'weight_max', value = weighting_values[2])
			# updateNumericInput(session, 'weight_step', value = weighting_values[3])
			
			updateTextInput(session, 'thresholds', value = r_data$json$thresholds)
			# updateTextInput(session, 'include_stands', value = r_data$json$include_stands) This doesn't exist yet
			
			############################
			get_inverse_select <- get_priority_output_names(reactive_priorities_fields()) # Find the 'inverse' target selection 
			# Populate outputs now that we have some options
			choices <- c(colnames(r_data$data), reactive_pcp_spm_fields(), reactive_pa_unit_field()) # Add non-input data options to choices
			select <- c(reactive_output_field(), get_inverse_select) # Get previous selections if needed and add new
			updateSelectInput(session, 'outputs_select', choices = choices, selected = select)
			############################

			updateSelectInput(session, 'outputs_select', selected = r_data$json$output_fields)
			updateSelectInput(session, 'grouping_variables', selected = r_data$json$grouping_variables)
			# updateTextInput(session, 'fixed_target', value = r_data$json$fixed_target)
			
			updateNumericInput(session, 'fixed_area_target', value = r_data$json$fixed_area_target)
			updateCheckboxInput(session, 'system_constraint', value = r_data$json$system_constraint)
			updateCheckboxInput(session, 'overwrite_output_chk', value = r_data$json$overwrite_output)
		}
		})

	# Populate the PCP and SPM field selector
	reactive_pcp_spm_fields <- reactive({
		fields <- input$pcp_spm_fields
		})

	reactive_pa_unit_field <- reactive({
		fields <- input$pa_unit_field
		})

	reactive_output_field <- reactive({
		choices <- input$outputs_select
		})

	reactive_priorities_fields <- reactive({
		choices <- input$priorities_fields
		})


	# Event listener for the pcp_spm_fields which are the fields that will have PCP and SPM calculated for them
	# In other words, the STND fields (probably)
	observeEvent(reactive_pcp_spm_fields(), {
		# Populate outputs now that we have some options
		choices <- colnames(r_data$data) # In case this hasn't been populated before, get all colnames
		select <- c(reactive_output_field(), reactive_pcp_spm_fields(), reactive_pa_unit_field()) # In case a previous selection was made, get them
		updateSelectInput(session, 'outputs_select', choices = choices, selected = select)

		# Now create the spm_pcp (priorities) choices (these aren't fields from the input data)
		choices <- get_spm_pcp_names(reactive_pcp_spm_fields())
		updateSelectInput(session, 'priorities_fields', choices = choices)
		})

	# Event listener for priorities_fields selector. Options are generated from pcp_spm_fields and are not
	# part of the input data. So, add the created options in addition to input options to outputs_select
	observeEvent(reactive_priorities_fields(), {
		get_inverse_select <- get_priority_output_names(reactive_priorities_fields()) # Find the 'inverse' target selection 
		# Populate outputs now that we have some options
		choices <- c(colnames(r_data$data), reactive_pcp_spm_fields(), reactive_pa_unit_field()) # Add non-input data options to choices
		select <- c(reactive_output_field(), get_inverse_select) # Get previous selections if needed and add new
		updateSelectInput(session, 'outputs_select', choices = choices, selected = select)
		})

	# Event listener for the pa_unit field and adds it to output choices
	observeEvent(reactive_pa_unit_field(), {
		choices <- c(colnames(r_data$data), reactive_pcp_spm_fields(), reactive_pa_unit_field()) # Add non-input data options to choices
		select <- c(reactive_output_field(), reactive_pa_unit_field())
		updateSelectInput(session, 'outputs_select', choices = choices, selected = select)
		})

	
	# Event listener for saving a scenario but not running. It will serialize user selections and save them to a json file
	observeEvent(input$save_scenario_but, {

		weight_values <- weight_values_to_string(input$weight_min, input$weight_max, input$weight_step)

		json <- write_save_file(
			scenario_name = input$scenario_name, 
			input_standfile = r_data$data_path,
			write_stand_outputs = input$write_stand_outputs_chk, 
			stand_field = input$stand_field,
			pcp_spm = input$pcp_spm_fields,
			land_base = input$land_base_field,
			priorities = input$priorities_fields, 
			stand_group_by = input$stand_group_by_field,
			pa_target = input$pa_target_field,
			pa_unit = input$pa_unit_field,
			pa_target_multiplier = input$pa_target_multiplier,
			nesting = FALSE,
			nesting_group_by = NULL,
			nesting_target = NULL,
			nesting_unit = NULL,
			nesting_target_multiplier = 1.0,
			weighting_values = weight_values,
			thresholds = input$thresholds_expr,
			include_stands = c("man_alldis == 1"), # TODO parse include_stands from thresholds, or the other way around
			output_fields = input$outputs_select,
			grouping_variables = input$grouping_fields, # c("PA_ID", "Owner"),
			fixed_target = FALSE,
			fixed_area_target = input$fixed_area_target,
			system_constraint = input$system_constraint_chk,
			overwrite_output = input$overwrite_output_chk
			)

		r_data$json <- json

		available_scenarios <- list_scenarios()
		updateSelectInput(session, 'select_scenario', choices = available_scenarios)
		updateTabsetPanel(session, 'main_nav', selected = 'simulation_panel')
		})

	observeEvent(input$save_and_run_but, {
		weight_values <- weight_values_to_string(input$weight_min, input$weight_max, input$weight_step)

		json <- write_save_file(
			scenario_name = input$scenario_name, 
			input_standfile = r_data$data_path,
			write_stand_outputs = input$write_stand_outputs_chk, 
			stand_field = input$stand_field,
			pcp_spm = input$pcp_spm_fields,
			land_base = input$land_base_field,
			priorities = input$priorities_fields, 
			stand_group_by = input$stand_group_by_field,
			pa_target = input$pa_target_field,
			pa_unit = input$pa_unit_field,
			pa_target_multiplier = input$pa_target_multiplier,
			nesting = FALSE,
			nesting_group_by = NULL,
			nesting_target = NULL,
			nesting_unit = NULL,
			nesting_target_multiplier = 1.0,
			weighting_values = weight_values,
			thresholds = input$thresholds_expr,
			include_stands = c("man_alldis == 1"), # TODO parse include_stands from thresholds, or the other way around
			output_fields = input$outputs_select,
			grouping_variables = input$grouping_fields, # c("PA_ID", "Owner"),
			fixed_target = FALSE,
			fixed_area_target = input$fixed_area_target,
			system_constraint = input$system_constraint_chk,
			overwrite_output = input$overwrite_output_chk
			)

		r_data$json <- json

		available_scenarios <- list_scenarios()
		updateSelectInput(session, 'select_scenario', choices = available_scenarios)
		req(updateTabsetPanel(session, 'main_nav', selected = 'simulation_panel'))

		output$simulation_output <- renderPrint({
			run(
				scenario_name = input$scenario_name, 
				input_standfile = r_data$data_path,
				write_stand_outputs = input$write_stand_outputs_chk, 
				stand_field = input$stand_field,
				pcp_spm = input$pcp_spm_fields,
				land_base = input$land_base_field,
				priorities = input$priorities_fields, 
				stand_group_by = input$stand_group_by_field,
				pa_target = input$pa_target_field,
				pa_unit = input$pa_unit_field,
				pa_target_multiplier = input$pa_target_multiplier_field,
				nesting = FALSE,
				nesting_group_by = NULL,
				nesting_target = NULL,
				nesting_unit = NULL,
				nesting_target_multiplier = 1.0,
				# weighting_values = weight_values,
				# thresholds = input$thresholds_expr,
				# include_stands = c("man_alldis == 1"), # TODO parse include_stands from thresholds, or the other way around
				output_fields = input$outputs_select,
				grouping_variables = input$grouping_fields, # c("PA_ID", "Owner"),
				fixed_target = FALSE,
				fixed_area_target = input$fixed_area_target,
				system_constraint = input$system_constraint_chk,
				overwrite_output = input$overwrite_output_chk
				)
			})
		})

	# TODO add the rest of the fields to clear
	observeEvent(input$clear_form_but, {
		updateTextInput(session, 'scenario_name', value = '')

		r_data$data <- NULL
		r_data$data_path <- NULL
		r_data$exp <- NULL
		r_data$json <- ''
		reset('file_select')

		updateCheckboxInput(session, 'write_stand_outputs_chk', value = FALSE)
		updateSelectInput(session, 'pcp_spm_fields', choices = NULL)
		updateSelectInput(session, 'land_base_field', choices = NULL)
		updateSelectInput(session, 'priorities_fields', choices = NULL)
		updateSelectInput(session, 'stand_group_by_field', choices = NULL)
		updateSelectInput(session, 'pa_target_field', choices = NULL)
		updateSelectInput(session, 'pa_unit_field', choices = NULL)
		updateSelectInput(session, 'pa_target_multiplier_field', choices = NULL)

		})

	# Event listener for loading a scenario. Will deserialize a json file created from one of the save functions
	observeEvent(input$load_scenario_but, {
		validate(
			need(input$select_scenario == '', 'Please select a scenario to load')
			)

		# Block loading dataset until on the right page. It seems like if we're not on the right tab the 
		# selected values don't update correctly
		updateTabsetPanel(session, 'main_nav', selected = 'scenario_setup_panel')

		json_data = read_save_file(input$select_scenario)

		file <- json_data$input_standfile
		ext <- tools::file_ext(file)
		
		r_data$data <- switch(ext, 
			csv = load_dataset(file, FALSE), 
			dbf = load_dataset(file, TRUE))
		r_data$data_path <- file
		r_data$ext <- ext
		r_data$json <- json_data
		
		})

	observeEvent(input$delete_scenario_but, {
		file.remove(input$select_scenario)

		available_scenarios <- list_scenarios()
		updateSelectInput(session, 'select_scenario', choices = available_scenarios)
		})

}
