options(shiny.maxRequestSize = 2000*1024^2)

server <- function(input, output, session) {

	# TODO These tooltips don't work
	addTooltip(session, id = 'save_scenario_button', title = 'Save Scenario')
	addTooltip(session, id = 'save_and_run_but', title = 'Save and Run Scenario')
	addTooltip(session, id = 'clear_form_but', title = 'Clear Form')

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

	reactive_pcp_spm_fields <- reactive({
		fields <- input$pcp_spm_fields
		})

	observeEvent(reactive_pcp_spm_fields(), {
		choices <- get_spm_pcp_names(reactive_pcp_spm_fields())
		updateSelectInput(session, 'priorities_fields', choices = choices)
		})


	# output$input_data <- renderTable({
	# 	file <- input$file_select
	# 	ext <- tools::file_ext(file$datapath)

	# 	req(file)
	# 	validate(need(ext == 'dbf', 'Please upload a dbf file'))

	# 	load_dataset(file$datapath, TRUE)
	# 	})

}
