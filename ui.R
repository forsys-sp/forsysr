scenario_buttons <- fluidRow(
	column(4, align = 'center', 
		# actionButton('save_scenario_but', 'Save Scenario', icon = icon('save'))), 
		actionButton('save_scenario_but', '', icon = icon('save'))), 
	column(4, align = 'center', 
		# actionButton('save_and_run_but', 'Save and Run Scenario', icon = icon('refresh'))), 
		actionButton('save_and_run_but', '', icon = icon('refresh'))), 
	column(4, align = 'center', 
		# actionButton('clear_form_but', 'Clear Form', icon = icon('times')))
		actionButton('clear_form_but', '', icon = icon('times')))
	)

scenario_params_panel <- tabPanel(
	'Simulation',
	titlePanel('Setup Forsys'), 
	sidebarLayout(
		sidebarPanel(
			scenario_buttons, 
			hr(),
			textInput('scenario_name', 'Scenario Name'),
			
			tabsetPanel(
				tabPanel(id = 'flat_file_panel', 'Flat File Input', 
					br(), 
					fileInput(
						'file_select', 
						'Select Input Data', 
						accept = c('.csv', '.shp', '.dbf'))
					),
				tabPanel(id = 'gdb_file_panel', 'Geodatabase Input', 
					br(), 
					fileInput(
						'gdb_file_select', 
						'Select Input Geodatabase', 
						accept = c('.gdb')),
					selectInput('gdb_layer_select', 'Choose Layer', choice = NULL, multiple = FALSE, selectize = TRUE)
					)
				),

			checkboxInput('write_stand_outputs_chk', 'Write Stand Outputs'),
			
			selectInput('pcp_spm_fields', 'Select Fields to calculate PCP and SPM on', choices = NULL, multiple = TRUE, selectize = TRUE),

			selectInput('land_base_field', 'Select land_base Field', choices = NULL, multiple = FALSE, selectize = TRUE),

			selectInput('priorities_fields', 'Select Priorties', choices = NULL, multiple = TRUE, selectize = TRUE), 

			selectInput('stand_group_by_field', 'Select Stand Grouping Field', choices = NULL, multiple = FALSE, selectize = TRUE), 

			selectInput('pa_target_field', 'Choose PA Target', choices = NULL, multiple = FALSE, selectize = TRUE), 

			selectInput('pa_unit_field', 'Choose PA Target', choices = NULL, multiple = FALSE, selectize = TRUE), 

			selectInput('pa_target_multiplier_field', 'Choose PA Target', choices = NULL, multiple = FALSE, selectize = TRUE), 

			# TODO set up nesting 
			# selectInput('nesting_field', 'Choose PA Target', choices = NULL, multiple = FALSE, selectize = TRUE), 

			fluidRow(
				column(4,
					numericInput('weight_min', 'Minimum Weight', 0, width = '150px')), 
				column(4,
					numericInput('weight_max', 'Maximum Weight', 5, width = '150px')), 
				column(4,
					numericInput('weight_step', 'Weight Step', 1, width = '150px'))
			),
			# fluidRow(
			# 	column(4,
			# 		numericInput('weight_min', '', 0, width = '150px')), 
			# 	column(4,
			# 		numericInput('weight_max', 'Maximum Weight', 5, width = '150px')), 
			# 	column(4,
			# 		numericInput('weight_step', 'Weight Step', 1, width = '150px'))
			# ),
			
			textInput('thresholds_expr', 'Enter thresholds in the form "Manageable man_alldis == 1"'),

			selectInput('outputs_select', 'Choose Outputs', choices = NULL, multiple = TRUE, selectize = TRUE), 

			selectInput('grouping_fields', 'Choose fields to group by', choices = NULL, multiple = TRUE, selectize = TRUE),

			tabsetPanel(
				tabPanel(id = 'fixed_area_panel', 'Fixed Area', 
					br(),
					numericInput('fixed_area_target', 'Fixed Area Target', 2000)),
				tabPanel(id = 'variable_area_panel', 'Variable Area', 
					# TODO what do we need to define, if anything, for variable area?
					br(),
					numericInput('fixed_area_target', 'Variable Area Param', 2000))
				),

			checkboxInput('system_constraint_chk', 'If the constraint is by master nesting unit (i.e. treat the top X planning areas in each national forest), set FALSE. If the constraint is by the system (i.e. go to the best planning area regardless of where it is located), set TRUE.'), 
			checkboxInput('overwrite_output_chk', 'Overwrite Outputs'),

			hr(),
			scenario_buttons
			),
		mainPanel(
			#textOutput('input_data_path'),
			dataTableOutput('input_data')
			)
		)
	)


scenario_planning_main_panel <- navbarMenu(
	'Scenario Planning', 
	tabPanel(
		id = 'select_scenario_panel',
		'Access Saved Scenarios',
		titlePanel('Select from the following scenarios'),
		mainPanel(
			fluidRow(
				column(6,
					selectInput('select_scenario', '', c('Scenario 1', 'Scenario 2', 'Other Scenario'), size = 10, selectize = FALSE)
					),
				column(6,
					br(), 
					actionButton('load_scenario_but', 'Load Selected Scenario', class = 'btn-sm', icon = icon('sync-alt'), width = '200px'),
					br(), 
					actionButton('delete_scenario_but', 'Delete Selected Scenario', class = 'btn-sm', icon = icon('trash-alt'), width = '200px')
					)
				)
			)
		),
	tabPanel(
		id = 'create_scenario_from_scratch_panel',
		'Create New Scenario From Scratch',
		scenario_params_panel
		),
	tabPanel(
		id = 'create_scenario_from_config_panel',
		'Create New Scenario By Uploading Config',
		fileInput('config_input', 'Select Config File', accept = c('.json'))
		)
	)

simulation_main_panel <- tabPanel(
	'Simulation'
	)

results_main_panel <- tabPanel(
	'Analysis', 'contents'
	)





ui <- fluidPage(
  navbarPage(
	 title = "ForSys",
	 scenario_planning_main_panel,
	 simulation_main_panel,
	 results_main_panel
  )
)