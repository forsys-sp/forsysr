welcome_panel <- tabPanel(
'Welcome',
mainPanel(
	fluidRow(
		h1('Welcome to forsys.app'),
		p('forsys.app implements Lorem ipsum dolor sit amet, consectetur adipiscing elit. Nullam porta augue vitae leo imperdiet accumsan. Suspendisse viverra ullamcorper suscipit. Nunc quis massa augue. Integer bibendum porta pellentesque. Proin volutpat mi auctor cursus convallis. Vestibulum sit amet lorem nunc. Sed sit amet lacus at urna feugiat hendrerit. Nulla dapibus lacinia sapien, nec commodo turpis rhoncus vitae. Sed vehicula fermentum enim sit amet suscipit. Nunc dictum augue vel nulla malesuada, eget eleifend libero bibendum. Suspendisse at neque consequat, interdum metus id, suscipit ante. Nam eu lobortis purus, nec pretium massa. Pellentesque nulla dolor, porttitor sit amet sapien nec, lobortis porta diam.')
		),
	br(),
	fluidRow(
		column(4, align = 'center',
			actionButton('go_to_saved_scenario_but', 'Load Scenario', icon=icon('save'))),
		column(4, align = 'center',
			actionButton('go_to_new_scenario_but', 'New Scenario', icon=icon('plus-square'))),
		column(4, align = 'center',
			actionButton('get_help_but', 'Get Help', icon=icon('question')))
		)
	)
)

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


scenario_setup_panel_advanced <- tabPanel(
	value = 'scenario_setup_panel_advanced',
	'Guided Mode',
	# hr(),
	scenario_buttons_text,
	br(),
	fluidRow(
		# checkboxInput('extra_help_chk', 'Extra Help')
		),
	tabsetPanel(
		id = 'sspa_nav',
		tabPanel(
			'Basics',
			value = 'sspa_basics',
			br(),
			div(
				class = 'extra_help',
				p('Start Here. Enter a name for your scenario and select a data file to upload.')
				),
			textInput('scenario_name', 'Scenario Name'),

			div(
				class = 'extra_help',
				p('The following file formats are accepted: DBF, CSV, and ZIP (for shapefiles)')
				),
			tabsetPanel(
				tabPanel(id = 'flat_file_panel', 'File Input',
					br(),
					fileInput(
						'file_select',
						'Select Input Data',
						accept = c('.csv', '.dbf', '.zip'),
						multiple = TRUE)
					)
				),

			),
		tabPanel(
			'Data Preview',
			dataTableOutput('input_data')
			), #tabPanel
		tabPanel(
			'Priorities',
			value = 'sspa_priorities',
			br(),
			selectInput('stand_id_field', 'Stand ID Field', choices = NULL, multiple = FALSE, selectize = TRUE),
			selectInput('pcp_spm_fields', 'Fields to calculate PCP and SPM on', choices = NULL, multiple = TRUE, selectize = TRUE),
			selectInput('treatment_available_field', 'Available for Treatment Field', choices = NULL, multiple = FALSE, selectize = TRUE),
			selectInput('priorities_fields', 'Priorties', choices = NULL, multiple = TRUE, selectize = TRUE),

			div(
				id = 'weight_items',
				numericInput('weight_min', 'Minimum Weight', 0, width = '150px'),
				numericInput('weight_max', 'Maximum Weight', 5, width = '150px'),
				numericInput('weight_step', 'Weight Step', 1, width = '150px')
					)
				),
		tabPanel(
			'Planning Areas',
			value = 'sspa_pa',
			br(),
			selectInput('planning_unit_id_field', 'Planning Unit ID', choices = NULL, multiple = FALSE, selectize = TRUE),

			textInput('pa_target_field', 'Planning Unit Constraint', 'AREA_MAN'),

			radioButtons('variable_fixed_select', label = NULL, choices = c('Variable', 'Fixed'), inline = TRUE),

			tabsetPanel(
				tabPanel(id = 'variable_target_panel', 'Variable Constraint',
					br(),
					selectInput('pa_unit_field', 'Planning Unit Unit', choices = NULL, multiple = FALSE, selectize = TRUE),
					numericInput('pa_target_multiplier', 'Planning Unit Multiplier', 0.15)),
				tabPanel(id = 'fixed_target_panel', 'Fixed Constraint',
					br(),
					numericInput('fixed_target_value', 'Fixed Constraint Amount', 2000))
				),
			),
		tabPanel(
			'Administrative Units',
			value = 'sspa_nesting',
			br(),
			checkboxInput('use_au', 'Multiple Administrative Units'),
			div(
				id = 'au_items',
				selectInput('au_id_field', 'Administrative Unit ID', choices = NULL, multiple = FALSE, selectize = TRUE),
				textInput('au_target_field', 'Administrative Unit Constraint', 'AREA_MAN'),
				tabsetPanel(
					tabPanel(id = 'au_variable_target_panel', 'Variable Constraint',
						br(),
						selectInput('au_unit_field', 'Administrative Unit Unit', choices = NULL, multiple = FALSE, selectize = TRUE),
						numericInput('au_target_multiplier', 'Administrative Unit Constraint Multiplier', 1.0)),
					tabPanel(id = 'au_fixed_target_panel', 'Fixed Constraint',
						br(),
						numericInput('au_fixed_target_value', 'Fixed Constraint Amount', 2000))
				),
			),
		),
		tabPanel(
			'Thresholds',
			value = 'sspa_thresholds',
			br(),
			textAreaInput('thresholds_expr', label = 'Thresholds', value = 'Manageable man_alldis == 1', placeholder = 'Manageable man_alldis == 1'),
			radioButtons('thresholds_op', label = 'Thresholds Operator', choices = c('AND', 'OR'), inline = TRUE),
			), 
		tabPanel(
			'Output',
			value = 'sspa_output',
			br(),
			selectInput('outputs_select', 'Choose Outputs', choices = NULL, multiple = TRUE, selectize = TRUE),
			selectInput('output_grouping_fields', 'Aggregate results by', choices = NULL, multiple = TRUE, selectize = TRUE),
			checkboxInput('write_stand_outputs_chk', 'Write Stand Outputs'),
			checkboxInput('overwrite_output_chk', 'Overwrite Outputs'),
			)
		) # tabsetPanel
	)

scenario_select_main_panel <- navbarMenu(
	'Scenario Planning',
	tabPanel(
		value = 'select_scenario_panel',
		'Access Saved Scenarios',
		titlePanel('Select from the following scenarios'),
		mainPanel(
			selectInput('select_scenario', '', c(), size = 10, selectize = FALSE),
			br(),
			actionButton('load_scenario_but', 'Load Selected Scenario', class = 'btn-sm', icon = icon('sync-alt'), width = '200px'),
			br(),
			actionButton('delete_scenario_but', 'Delete Selected Scenario', class = 'btn-sm', icon = icon('trash-alt'), width = '200px')
			)
		),
	tabPanel(
		id = 'create_scenario_from_config_panel',
		'Create New Scenario By Uploading Config',
		fileInput('config_input', 'Select Config File', accept = c('.json'))
		)
	)

scenario_setup_main_panel <- navbarMenu(
	'Scenario Setup',
	# scenario_setup_panel_simple,
	scenario_setup_panel_advanced)

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
			plotOutput('analysis_plot', click = 'results_plot_click'),
			leafletOutput("pa_map")
			)
		)
	)





ui <- fluidPage(
	shinyjs::useShinyjs(),
  	navbarPage(
	  	id = 'main_nav',
  		title = "forsys.app",

	  	welcome_panel,
  		scenario_select_main_panel,
  		scenario_setup_main_panel,
  		simulation_main_panel,
  		results_main_panel
  	)
)
