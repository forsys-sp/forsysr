
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

scenario_buttons_text<- fluidRow( 
	column(4, align = 'center', 
		actionButton('save_scenario_but', 'Save Scenario', icon = icon('save'))), 
	column(4, align = 'center', 
		actionButton('save_and_run_but', 'Save and Run Scenario', icon = icon('refresh'))), 
	column(4, align = 'center', 
		actionButton('clear_form_but', 'Clear Form', icon = icon('times')), 
		tags$script('
			Shiny.addCustomMessageHandler("resetFileInputHandler", function(x) {
				var id = "#" + x + "_progress"; 
				var idBar = id + " .bar"; 
				$(id).css("visibility", "hidden");
				$(idBar).css("width", "0%"); 
				});
			')
		)
	)

scenario_params_panel <- tabPanel(
	'Scenario Setup',
	# hr(), 
	scenario_buttons_text, 
	br(),
	tabsetPanel(
		tabPanel(
			id = 'setup_scenario_panel',
			'Scenario Setup', 
			fluidRow(
				column(4,
					br(),
					textInput('scenario_name', 'Scenario Name'),
					
					tabsetPanel(
						tabPanel(id = 'flat_file_panel', 'File Input', 
							br(), 
							fileInput(
								'file_select', 
								'Select Input Data', 
								accept = c('.csv', '.dbf', '.zip'), 
								multiple = TRUE)
							)#,
						# tabPanel(id = 'gdb_file_panel', 'Geodatabase Input', 
						# 	br(), 
						# 	fileInput(
						# 		'gdb_file_select', 
						# 		'Select Input Geodatabase', 
						# 		accept = c('.gdb')),
						# 	selectInput('gdb_layer_select', 'Choose Layer', choice = NULL, multiple = FALSE, selectize = TRUE)
						# 	)
						),

					hr(), 

					selectInput('stand_id_field', 'Stand ID Field', choices = NULL, multiple = FALSE, selectize = TRUE), 
					
					selectInput('pcp_spm_fields', 'Fields to calculate PCP and SPM on', choices = NULL, multiple = TRUE, selectize = TRUE),

					selectInput('treatment_available_field', 'Available for Treatment Field', choices = NULL, multiple = FALSE, selectize = TRUE),

					selectInput('priorities_fields', 'Priorties', choices = NULL, multiple = TRUE, selectize = TRUE), 

					# fluidRow(
					# 	column(4,
					# 		numericInput('weight_min', 'Minimum Weight', 0, width = '150px')), 
					# 	column(4,
					# 		numericInput('weight_max', 'Maximum Weight', 5, width = '150px')), 
					# 	column(4,
					# 		numericInput('weight_step', 'Weight Step', 1, width = '150px'))
					# ),
					
					div(
						id = 'weight_items', 
						numericInput('weight_min', 'Minimum Weight', 0, width = '150px'), 
						numericInput('weight_max', 'Maximum Weight', 5, width = '150px'), 
						numericInput('weight_step', 'Weight Step', 1, width = '150px')
						)
					), 

				column(4, 

					br(), 

					selectInput('planning_unit_id_field', 'Planning Unit ID', choices = NULL, multiple = FALSE, selectize = TRUE), 

					textInput('pa_target_field', 'Planning Unit Constraint', 'AREA_MAN'),
					

					tabsetPanel(
						tabPanel(id = 'variable_target_panel', 'Variable Constraint', 
							br(),
							selectInput('pa_unit_field', 'Planning Unit Unit', choices = NULL, multiple = FALSE, selectize = TRUE), 
							numericInput('pa_target_multiplier', 'Planning Unit Multiplier', 0.15)),
						tabPanel(id = 'fixed_target_panel', 'Fixed Constraint', 
							br(),
							numericInput('fixed_target_value', 'Fixed Constraint Amount', 2000))
						),

					hr(), 
					checkboxInput('use_au', 'Multiple Administrative Units'), 
					div(
						id = 'au_items', 
						selectInput('au_id_field', 'Administrative Unit ID', choices = NULL, multiple = FALSE, selectize = TRUE), 
						textInput('au_target_field', 'Administrative Unit Constraint', 'AREA_MAN'), 
						selectInput('au_unit_field', 'Administrative Unit Unit', choices = NULL, multiple = FALSE, selectize = TRUE), 
						numericInput('au_target_multiplier', 'Administrative Unit Multiplier', 1.0)
						),
					), 
					

				column(4, 
					br(), 

					textAreaInput('thresholds_expr', 'Thresholds, one per line in the form "Manageable man_alldis == 1"'),
					radioButtons('threshold_operator', 'Thresholds Operator', choices = c('AND', 'OR'), inline = TRUE), 

					hr(), 

					selectInput('outputs_select', 'Choose Outputs', choices = NULL, multiple = TRUE, selectize = TRUE), 

					selectInput('output_grouping_fields', 'Aggregate results by', choices = NULL, multiple = TRUE, selectize = TRUE),

					checkboxInput('write_stand_outputs_chk', 'Write Stand Outputs'),

					checkboxInput('overwrite_output_chk', 'Overwrite Outputs'),

					hr()
					
					) # column
				) # fluidRow
			), # tabPanel
		tabPanel(
			'Data Preview', 
			dataTableOutput('input_data')
			) #tabPanel
		) # tabsetPanel
	) # tabPanel


scenario_planning_main_panel <- navbarMenu(
	'Scenario Planning', 
	tabPanel(
		value = 'select_scenario_panel',
		'Access Saved Scenarios',
		titlePanel('Select from the following scenarios'),
		mainPanel(
			fluidRow(
				column(6,
					selectInput('select_scenario', '', c(), size = 10, selectize = FALSE)
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
		id = 'create_scenario_from_config_panel',
		'Create New Scenario By Uploading Config',
		fileInput('config_input', 'Select Config File', accept = c('.json'))
		)
	)

scenario_setup_panel <- tabPanel(
	value = 'scenario_setup_panel',
	'Set Scenario Parameters',
	scenario_params_panel
	)

simulation_main_panel <- tabPanel(
	value = 'simulation_panel', 
	'Simulation', 
	textOutput('simulation_output')
	)

results_main_panel <- tabPanel(
	value = 'analysis_panel',
	'Results and Analysis', 
	sidebarLayout(
		sidebarPanel(
			h3('Charts'),
			# p(actionLink('attainment_efficiency', 'Attainment Efficiency')),
			p(actionLink('attainment_efficiency_by_area','Attainment Efficiency by Area Treated')),
			p(actionLink('production_frontiers','Production Frontiers')),
			h3('Maps'),
			p('Treatment Prioritization'),
			h3('Data'), 
			p(downloadLink('download_data','Download Data')),
			p(downloadLink('download_pdf','Download Analysis PDF')),
			),
		mainPanel(
			# selectInput('stand_id_field', 'Stand ID Field', choices = NULL, multiple = FALSE, selectize = TRUE), 
			selectInput('plot_x_field', 'Select Planning Unit Target', choices = NULL, multiple = FALSE, selectize = TRUE),
			selectInput('plot_priority', 'Select Priority', choices = NULL, multiple = FALSE, selectize = TRUE),
			plotOutput('analysis_plot', click = 'results_plot_click')

			)
		)
	)





ui <- fluidPage(
	shinyjs::useShinyjs(), 
  	navbarPage(
	  	id = 'main_nav',
		title = "ForSys",

		scenario_planning_main_panel,
		scenario_setup_panel, 
		simulation_main_panel,
		results_main_panel
  	)
)