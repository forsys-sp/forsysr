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


	data <- reactive({
		# Block until file is completely uploaded
		req(input$file_select)

		# fileInput returns an object with parameters
		file <- input$file_select
		ext <- tools::file_ext(file$datapath)
		
		# Remember R returns the last thing assigned to, no return needed
		switch(ext, 
			csv = load_dataset(file$datapath, FALSE), 
			dbf = load_dataset(file$datapath, TRUE))
		})

	output$input_data <- renderDataTable({
		head(data(), 10)
		})

	# Update field select boxes after data is loaded
	# TODO maintain a list of already selected columns and remove from available list.
	# this will require catching un-selection of fields too to add back to list
	observeEvent(data(), {
		choices <- colnames(data())
		updateSelectInput(session, 'pcp_spm_fields', choices = choices)
		updateSelectInput(session, 'land_base_field', choices = choices)
		updateSelectInput(session, 'stand_group_by_field', choices = choices)
		updateSelectInput(session, 'pa_target_field', choices = choices)
		updateSelectInput(session, 'pa_unit_field', choices = choices)
		updateSelectInput(session, 'pa_target_multiplier_field', choices = choices)

		})

	# Populate the PCP and SPM field selector
	reactive_pcp_spm_fields <- reactive({
		fields <- input$pcp_spm_fields
		})

	observeEvent(reactive_pcp_spm_fields(), {
		choices <- get_spm_pcp_names(reactive_pcp_spm_fields())
		updateSelectInput(session, 'priorities_fields', choices = choices)
		})


	observeEvent(input$save_scenario_but, {
		updateTabsetPanel(session, 'main_nav', selected = 'simulation_panel')
		})

	observeEvent(input$save_and_run_but, {
		updateTabsetPanel(session, 'main_nav', selected = 'simulation_panel')
		})

	observeEvent(input$clear_form_but, {
		updateTextInput(session, 'scenario_name', value = '')
		# updateFileInput(session, 'file_select', value = '') need to use js to clear this
		updateCheckboxInput(session, 'write_stand_outputs_chk', value = FALSE)
		updateSelectInput(session, 'pcp_spm_fields', choices = NULL)
		updateSelectInput(session, 'land_base_field', choices = NULL)
		updateSelectInput(session, 'priorities_fields', choices = NULL)
		updateSelectInput(session, 'stand_group_by_field', choices = NULL)
		updateSelectInput(session, 'pa_target_field', choices = NULL)
		updateSelectInput(session, 'pa_unit_field', choices = NULL)
		updateSelectInput(session, 'pa_target_multiplier_field', choices = NULL)

		})




 #  scenario_name = '',
 #  input_standfile = '',
 #  stand_field = 'Cell_ID',
 #  pcp_spm = c(),
 #  land_base = '',
 #  priorities = c(),
 #  stand_group_by = '',
 #  pa_target = '',
 #  pa_unit = '',
 #  pa_target_multiplier = 0.15,
 #  nesting = FALSE,
 #  nesting_group_by = NULL,
 #  nesting_target = NULL,
 #  nesting_unit = NULL,
 #  nesting_target_multiplier = 1.0,
 #  weighting_values = "0 5 1",
 #  thresholds = c("Manageable man_alldis == 1") ,
 #  include_stands = c("man_alldis == 1"),
 #  output_fields = c("AREA_HA", "TVMBF_STND", "TVMBF_PCP", "HUSUM_STND", "HUSUM_PCP"),
 #  grouping_variables = c("PA_ID", "Owner"),
 #  fixed_target = FALSE,
 #  fixed_area_target = 2000,
 #  system_constraint = FALSE,
 #  overwrite_output = TRUE
	# save_data <- observeEvent(input$save_scenario_but, {
		
	# 	validate(
	# 		need(input$scenario_name != '', 'Please name the scenario'),
	# 		need(data() != NULL, 'Data not loaded properly, try uploading again'),
	# 		need(input$pcp_spm_fields != '', 'Please select at least one priority' )
	# 		)

	# 	write_save_file(
	# 		input$scenario_name, 
	# 		)
	# 	})

}
