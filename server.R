options(shiny.maxRequestSize = 2000*1024^2)


validate_inputs <- function() {

}

server <- function(input, output, session) {

	########################################################

					# Multiple Panel Variables

	########################################################


	# These are the main reactive data collections. 
	# r_data contains the input data itself, along with some information about the input data.
	# r_choices acts as a collection of possibe selections from the data to use with selectInput
	r_data <- reactiveValues(data = NULL, data_path = NULL, ext = NULL, json = '')
	r_choices <- reactiveValues(priorities = NULL, outputs = NULL)

	# r_data could be populated 2 ways: by user input, or by loading an old scenario. 
	# data() handles user uploaded data
	data <- reactive({
		# Block until file is completely uploaded
		req(input$file_select)


		print(input$file_select)
		print(class(input$file_select))

		apply(input$file_select, 1, function(x) {
			file <- x
			ext <- tools::file_ext(x['datapath'])

			perm_data_path <- paste0('data/', x['name'])
			file.copy(x['datapath'], perm_data_path)


			if (stringr::str_detect(ext, 'dbf') | stringr::str_detect(ext, 'csv')) {
				r_data$data_path <- perm_data_path
				r_data$ext <- ext


				# Remember R returns the last thing assigned to, no return needed
				r_data$data <- switch(ext, 
					csv = load_dataset(perm_data_path, FALSE), 
					dbf = load_dataset(perm_data_path, TRUE))
				}
			
			})
		})

	########################################################

					# Scenario Select Panel

	########################################################

	# TODO These tooltips don't work
	addTooltip(session, id = 'save_scenario_button', title = 'Save Scenario')
	addTooltip(session, id = 'save_and_run_but', title = 'Save and Run Scenario')
	addTooltip(session, id = 'clear_form_but', title = 'Clear Form')

	# Get the intially availabled scenarios from disk
	available_scenarios <- list_scenarios()
	updateSelectInput(session, 'select_scenario', choices = available_scenarios)

	# If there are no saved scenarios, then skip to the scenario creation page
	if (length(available_scenarios) == 0) {
		updateTabsetPanel(session, 'main_nav', selected = 'scenario_setup_panel')
	}

	# Event listener for loading a scenario. Will deserialize a json file created from one of the save functions
	observeEvent(input$load_scenario_but, {
		validate(
			need(input$select_scenario == '', 'Please select a scenario to load')
			)

		# Block loading dataset until on the right page. It seems like if we're not on the right tab the 
		# selected values don't update correctly
		updateTabsetPanel(session, 'main_nav', selected = 'scenario_setup_panel')

		# Read in the scenario data
		json_data = read_save_file(input$select_scenario)

		file <- json_data$input_standfile
		ext <- tools::file_ext(file)
		
		# Update the reactive data elements
		r_data$data <- switch(ext, 
			csv = load_dataset(file, FALSE), 
			dbf = load_dataset(file, TRUE))
		r_data$data_path <- file
		r_data$ext <- ext
		r_data$json <- json_data
		
		})

	# Event listener for the delete scenario button
	# TODO add a "are you sure" prompt
	observeEvent(input$delete_scenario_but, {
		file.remove(input$select_scenario)

		available_scenarios <- list_scenarios()
		updateSelectInput(session, 'select_scenario', choices = available_scenarios)
		})

	reactive_au_toggle <- reactive({
		t <- input$use_au
		})

	observeEvent(reactive_au_toggle(), {
		if (reactive_au_toggle())
		{
			shinyjs::show(id = 'au_items')
		} else {
			shinyjs::hide(id = 'au_items')
		}
		
		})


	########################################################

					# Scenario Setup Panel

	########################################################

	


	# Generate a preview of the data for the data preview page
	output$input_data <- renderDataTable({
		head(r_data$data, 10)
		})

	# Create a listener for user uploaded data. This actually fires the reactive data() loading process
	observeEvent(data(), {
		data()
		})

	# Event listener for the reactive data. This updates available selection options in selectInputs
	observeEvent(r_data$data, {
		choices <- colnames(r_data$data)
		updateSelectInput(session, 'stand_id_field', choices = choices)
		updateSelectInput(session, 'pcp_spm_fields', choices = choices)
		updateSelectInput(session, 'treatment_available_field', choices = choices)
		updateSelectInput(session, 'planning_unit_id_field', choices = choices)
		# updateSelectInput(session, 'pa_target_field', choices = choices) # TODO in the Idaho file, this is "AREA_MAN" which doesn't exist in the data
		updateSelectInput(session, 'pa_unit_field', choices = choices)
		updateSelectInput(session, 'output_grouping_fields', choices = choices)
		updateSelectInput(session, 'au_id_field', choices = choices)
		updateSelectInput(session, 'au_target_multiplier', choices = choices)

		# print(r_data$json)

		# TODO implement better check for json data
		# If there is json data available (so a saved scenario with user selections), attempt to update
		# selectInput boxes with old selections
		if (length(r_data$json) > 1) {
			updateTextInput(session, 'scenario_name', value = r_data$json$scenario_name)
			updateSelectInput(session, 'stand_id_field', selected = r_data$json$stand_id_field)
			updateSelectInput(session, 'pcp_spm_fields', selected = r_data$json$pcp_spm)

			# TODO priorities and grouping doesn't update properly. This probably has something to do with 
			# execution order of the reactive environments. Find a way around this.

			############################
			# Populate outputs now that we have some options
			select <- c(reactive_output_field(), reactive_pcp_spm_fields(), reactive_pa_unit_field()) # In case a previous selection was made, get them
			updateSelectInput(session, 'outputs_select', choices = choices, selected = select)

			# Now create the spm_pcp (priorities) choices (these aren't fields from the input data)
			choices <- get_spm_pcp_names(reactive_pcp_spm_fields())
			updateSelectInput(session, 'priorities_fields', choices = choices, selected = r_data$json$priorities)
			############################


			updateSelectInput(session, 'treatment_available_field', selected = r_data$json$land_base)
			updateSelectInput(session, 'planning_unit_id_field', selected = r_data$json$stand_group_by)
			updateTextInput(session, 'pa_target_field', value = r_data$json$pa_target)
			updateSelectInput(session, 'pa_unit_field', selected = r_data$json$pa_unit)
			updateNumericInput(session, 'pa_target_multiplier', value = r_data$json$pa_target_multiplier)
			
			updateTextInput(session, 'use_au', value = r_data$json$nesting)
			updateTextInput(session, 'au_id_field', value = r_data$json$nesting_group_by)
			updateTextInput(session, 'au_target_field', value = r_data$json$nesting_target)
			updateTextInput(session, 'au_unit_field', value = r_data$json$nesting_unit)
			updateTextInput(session, 'au_target_multiplier', value = r_data$json$au_target_multiplier)

			weighting_values <- weight_string_to_values(r_data$json$weighting_values)
			updateNumericInput(session, 'weight_min', value = weighting_values[1])
			updateNumericInput(session, 'weight_max', value = weighting_values[2])
			updateNumericInput(session, 'weight_step', value = weighting_values[3])
			
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
			updateSelectInput(session, 'output_grouping_fields', selected = r_data$json$grouping_variables)
			# updateTextInput(session, 'fixed_target', value = r_data$json$fixed_target)
			
			updateNumericInput(session, 'fixed_target_value', value = r_data$json$fixed_target_value)

			updateCheckboxInput(session, 'write_stand_outputs_chk', value = r_data$json$write_stand_outputs)
			updateCheckboxInput(session, 'overwrite_output_chk', value = r_data$json$overwrite_output)
		}
		})

	# Certain fields can affect which options are available for other fields. These functions set up
	# a reactive retrieval of these selections
	reactive_pcp_spm_fields <- reactive({
		fields <- input$pcp_spm_fields
		})

	reactive_pa_unit_field <- reactive({
		fields <- input$pa_unit_field
		})

	reactive_output_field <- reactive({
		selected <- input$outputs_select
		})

	reactive_priorities_fields <- reactive({
		selected <- input$priorities_fields
		})

	weights <- reactive({

		if (length(reactive_priorities_fields()) <= 1) {
			shinyjs::hide(id = 'weight_items')
		} else {
			shinyjs::show(id = 'weight_items')
		}

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

		# Run the weight hider/shower
		weights()
		})

	# Event listener for the pa_unit field and adds it to output choices
	observeEvent(reactive_pa_unit_field(), {
		choices <- c(colnames(r_data$data), reactive_pcp_spm_fields(), reactive_pa_unit_field()) # Add non-input data options to choices
		select <- c(reactive_output_field(), reactive_pa_unit_field())
		updateSelectInput(session, 'outputs_select', choices = choices, selected = select)
		})

	observeEvent(input$thresholds_expr, {
		line_count <- str_count(input$thresholds_expr, '\n') + 1
		if (line_count > 1) {
			shinyjs::show(id = 'threshold_operator')
		} else {
			shinyjs::hide(id = 'threshold_operator')
		}
		})

	
	# Event listener for saving a scenario but not running. It will serialize user selections and save them to a json file
	observeEvent(input$save_scenario_but, {

		if (length(reactive_priorities_fields()) <= 1) {
			updateNumericInput(session, 'weight_min', value = 1)
			updateNumericInput(session, 'weight_max', value = 1)
			updateNumericInput(session, 'weight_step', value = 1)
		} 

		json <- write_save_file_helper(input, r_data$data_path)

		r_data$json <- json

		available_scenarios <- list_scenarios()
		updateSelectInput(session, 'select_scenario', choices = available_scenarios)
		updateTabsetPanel(session, 'main_nav', selected = 'simulation_panel')
		})

	observeEvent(input$save_and_run_but, {

		if (length(reactive_priorities_fields()) <= 1) {
			updateNumericInput(session, 'weight_min', value = 1)
			updateNumericInput(session, 'weight_max', value = 1)
			updateNumericInput(session, 'weight_step', value = 1)
		} 

		json <- write_save_file_helper(input, r_data$data_path)

		r_data$json <- json

		available_scenarios <- list_scenarios()
		updateSelectInput(session, 'select_scenario', choices = available_scenarios)
		req(updateTabsetPanel(session, 'main_nav', selected = 'simulation_panel'))

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

		output$simulation_output <- renderPrint({
			run(
				scenario_name = input$scenario_name, 
				input_standfile = r_data$data_path,
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
				# weighting_values = weight_values,
				# thresholds = input$thresholds_expr,
				# include_stands = c("man_alldis == 1"), # TODO parse include_stands from thresholds, or the other way around
				output_fields = input$outputs_select,
				grouping_variables = input$output_grouping_fields, # c("PA_ID", "Owner"),
				fixed_target = FALSE,
				fixed_area_target = input$fixed_target_value,
				overwrite_output = input$overwrite_output_chk,
				shiny_output = TRUE
				)
			})
		})

	# Event listener for the clear panel button. Clears the scenario setup panel
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
		updateSelectInput(session, 'treatment_available_field', choices = NULL)
		updateSelectInput(session, 'priorities_fields', choices = NULL)
		updateSelectInput(session, 'planning_unit_id_field', choices = NULL)
		updateSelectInput(session, 'pa_target_field', choices = NULL)
		updateSelectInput(session, 'pa_unit_field', choices = NULL)

		})


	########################################################

					# Simulation Panel

	########################################################





	########################################################

					# Results Panel

	########################################################

	# results_files <- reactive({
	# 	output_files <- sapply(list.files('output'), function(x) glue('output/{x}'))
	# 	})

	result_data_all_name <- reactive({
		paste0(r_data$json$scenario_name, '/pa_all_', r_data$json$scenario_name, '.csv')
		})

	result_data <- reactive({
		p <- result_data_all_name()
		d <- readr::read_csv(p)
		})

	reactive_plot_value <- reactiveValues(plot = NULL)

	reactive_attainment_chart_by_target_treated <- reactive({
		p = 'output/NNA/pa_all_NNA.csv'
		d = readr::read_csv(p)

		target_field = 'ETrt_AREA_HA'
		pcp_field = 'ETrt_TVMBF_PCP'
		priority = 'Pr_1_TVMBF_SPM_SPM'

		attainment_chart_by_target_treated(d, pcp_field, target_field, priority)
		})

	observeEvent(input$attainment_efficiency_by_area, {
		output$analysis_plot <- renderPlot(reactive_attainment_chart_by_target_treated())
		})


	observeEvent(input$production_frontiers, {
		d <- result_data()
		})


	output$download_data <- downloadHandler(
	    filename = function() {
	      result_data_all_name()
	    },
	    content = function(file) {
	      write.csv(result_data(), file)
	    }
	  )
	

	
}
