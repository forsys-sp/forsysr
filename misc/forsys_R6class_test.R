pacman::p_load(R6, rlang, dplyr, tidyr)

data("test_forest")
test_forest_dat <- test_forest %>% st_drop_geometry() %>% dplyr::select(-Project, -DoTreat)

scenario_generator <- R6Class(
  "ScenarioGenerator",
  private = list(
    ..stands = NULL,
    ..projects = NULL
  ),
  public = list(
    initialize = function(stands){
      if(!missing(stands)){
        stands$selected = 0
        stands$treated = 0
        stands$proj_rank = NA
        private$..stands = stands
        out = data.frame(stand_id = self$get_stand_ids(), proj_id = NA)
        private$..projects <- out
      }
    },
    select_stands = function(stand_ids, proj_ids = NULL){
      x = (private$..stands$stand_id %in% stand_ids) & (private$..stands$selected == 0)
      private$..stands$selected[x] <- 1
      private$..stands$proj_rank[x] <- proj_ids
      message(glue::glue("{sum(x)} stands selected"))
    },
    deselect_stands = function(stand_ids){
      x = (private$..stands$stand_id %in% stand_ids) & (private$..stands$selected == 1)
      private$..stands$selected[x] <- 0
      private$..stands$proj_rank[x] <- NA
      message(glue::glue("{sum(x)} stands deselected"))
    },
    get_stand_ids = function(){
      unique(private$..stands$stand_id)
    },
    get_max_proj_id = function(){
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

a_scenario <- scenario_generator$new(stands = test_forest_dat)
a_scenario$select_stands(1:100, proj_ids = 1)
a_scenario$available %>% dim()
a_scenario$selected %>% dim()
a_scenario$select_stands(301:400, proj_ids = 2)
a_scenario$get_max_proj_id()
a_scenario$summarize_projs()

