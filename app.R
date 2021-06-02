library(shiny)

source('R/forsys_libraries.R')
source('R/forsys_functions.R')
source('R/forsys_scenario_functions.R')
source('R/forsys_results_functions.R')
source('R/ForSys.R')
source('ui.R')
source('server.R')

run('config_TenYearPlan_WW_FS.R', dynamic_forsys = T)

shinyApp(ui, server)
