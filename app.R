library(shiny)

source('R/forsys_libraries.R')
source('R/forsys_functions.R')
source('R/forsys_scenario_functions.R')
source('R/forsys_results_functions.R')
source('R/ForSys.R')
source('ui.R')
source('server.R')

run('configs/config_TenYearPlan_WW_FS.R')

shinyApp(ui, server)
