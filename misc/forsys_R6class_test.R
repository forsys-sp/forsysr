# load packages
pacman::p_load(R6, rlang, dplyr, tidyr, assertthat)

# scenario object generator (read this into memory)
scenario_generator <- R6Class(
  "ScenarioGenerator",
  private = list(
    ..stands = NULL
  ),
  public = list(
    initialize = function(stands){
      if(!missing(stands)){
        stands$selected = 0
        stands$treated = 0
        stands$proj_rank = 0
        private$..stands = stands
      }
    },
    select_stands = function(stand_ids, project = NULL, increment = TRUE, treated = NULL){
      x = (private$..stands$stand_id %in% stand_ids) & (private$..stands$selected == 0)
      private$..stands$selected[x] <- 1
      private$..stands$proj_rank[x] <- ifelse(is.null(project), self$get_max_proj() + 1, project)
      # private$..stands$proj_rank[x] <- project
      message(glue::glue("{sum(x)} stands selected"))
    },
    deselect_stands = function(stand_ids){
      x <- (private$..stands$stand_id %in% stand_ids) & (private$..stands$selected == 1)
      private$..stands$selected[x] <- 0
      private$..stands$proj_rank[x] <- 0
      message(glue::glue("{sum(x)} stands deselected"))
    },
    get_stand_ids = function(){
      unique(private$..stands$stand_id)
    },
    get_max_proj = function(){
      max(private$..stands$proj_rank, na.rm=T)
    },
    summarize_projs = function(grouping_vars=NULL){
      private$..stands %>%
        group_by(proj_rank, across({{grouping_vars}})) %>%
        drop_na(proj_rank) %>%
        summarize_if(is.numeric, sum, na.rm=T)
    }
  ),
  active = list(
    stands = function(){
      private$..stands
    },
    available = function(){
      private$..stands %>% dplyr::filter(selected == 0)
    },
    selected = function(){
      private$..stands %>% dplyr::filter(selected == 1)
    }
  )
)

# load data
load('data/test_forest.Rda')

# build new scenario object using teh scenario generator
a_scenario <- scenario_generator$new(stands = test_forest_data)
# select stands 1 through 100 and assign to project 1
a_scenario$select_stands(1:100)
# selects stands 51 through 130... notice how only new 50 stands are selected
a_scenario$select_stands(51:130, 1)
# deselect stands 51 through 95
a_scenario$deselect_stands(51:95)
# check to see which stands are available...
nrow(a_scenario$available)
# ... and which are selected
nrow(a_scenario$selected)
# selects stands 301 to 400 and assign to project 2
a_scenario$select_stands(500:600)
# summarize selected stands based on project ids
a_scenario$summarize_projs()

