#' Wrapper for building projects using predetermined project areas
#'
#' @param stands 
#' @param stand_id_field Field name containing unique stand id.  <emph{character}>
#' @param stand_area_field Field name containing stand area.  <emph{character}>
#' @param proj_id_field Field name containing unique project area id.  <emph{character}>
#' @param proj_target_field Field name used as primary constraint.  <emph{character}>
#' @param proj_fixed_target Whether project target is fixed or relative. <emph{logical}>
#' @param proj_target_value Either absolute value for target or relative percent (0-1) of project area sum.
#' @param proj_target_min_value TODO ???
#' @param stand_threshold Boolean state on whether stand is available for treatment. <emph{character}>
#' @param proj_number TODO ???
#' @param proj_area_ceiling TODO ???
#'
#' @importFrom dplyr rename select mutate arrange left_join inner_join
#' @importFrom tidyr drop_na
#'
build_static_projects <- function(
    stands,
    stand_id_field,
    stand_area_field,
    proj_id_field,
    proj_target_field,
    proj_fixed_target,
    proj_target_value,
    proj_target_min_value = NULL,
    stand_threshold,
    proj_number,
    proj_area_ceiling
){
  
  # stand selection based on predetermined projects
  stands <- stands %>%
    # set treatment target
    set_treatment_target( 
      proj_id_field = proj_id_field,
      proj_fixed_target = proj_fixed_target,
      proj_target_field = proj_target_field,
      proj_target_value = proj_target_value
    )
  
  # apply treatment thresholds
  stands <- stands %>%
    filter_stands( 
      filter_txt = stand_threshold,
      verbose = T
    )
  
  # select stands
  stands <- stands %>%
    apply_treatment( 
      stand_id_field = stand_id_field,
      proj_id_field = proj_id_field,
      proj_objective = 'weightedPriority',
      proj_target_field = proj_target_field,
      proj_target = 'master_target'
    )
  
  # clean up output
  stands_selected <- stands %>%
    select(stand_id_field, proj_id_field, stand_area_field, 'weightedPriority') %>%
    mutate(treated = 1)
  
  # group selected stands by project, summarize, and rank
  projects_selected <- stands_selected %>%
    create_grouped_dataset(
      grouping_vars = proj_id_field,
      summing_vars = c(stand_area_field, 'weightedPriority')) %>%
    base::replace(is.na(.), 0) %>%
    arrange(-weightedPriority) %>%
    mutate(treatment_rank = ifelse(weightedPriority > 0, 1:dplyr::n(), NA)) %>%
    drop_na(treatment_rank)
  
  # filter project level output by project number or treatment area ceiling
  proj_number = ifelse(proj_number %>% is.null, Inf, proj_number)
  proj_area_ceiling = ifelse(proj_area_ceiling %>% is.na, Inf, proj_area_ceiling)
  projects_selected_out <- projects_selected %>%
    filter(treatment_rank <= proj_number) %>%
    filter(proj_area_ceiling <= proj_area_ceiling)
  
  # create stand level output by joining with output projects
  join_y = stands_selected %>% 
    inner_join(
      y = projects_selected_out %>% select(proj_id_field, 'treatment_rank'), 
      by = proj_id_field) %>% 
    select(stand_id_field, treated)
  
  stands_selected_out <- stands %>%
    select(stand_id_field, proj_id_field, stand_area_field, weightedPriority) %>%
    left_join(join_y, stand_id_field) %>%
    mutate(treated = ifelse(treated %>% is.na, 0, 1))
  
  stands_selected_out <- stands_selected_out %>%
    inner_join(
      y = projects_selected_out %>% select(proj_id_field, 'treatment_rank'), 
      by = proj_id_field) %>%
    arrange(treatment_rank, stand_id_field)
  
  return(list(
    projects_selected_out,
    stands_selected_out
  ))
}

#' Threshold string statement parser
#'
#' @param stands TODO
#' @param proj_id_field TODO
#' @param proj_fixed_target TODO
#' @param proj_target_field TODO
#' @param proj_target_value TODO
#'
set_treatment_target <- function(
    stands,
    proj_id_field,
    proj_fixed_target,
    proj_target_field=NULL,
    proj_target_value=NULL
) {
  
  # target based on fixed total
  if(length(proj_fixed_target > 0)){
    if (proj_fixed_target == TRUE) {
      stands <- stands %>%
        set_fixed_target(
          target_value = proj_target_value
        )
    # target based on percent total of field
    } else if (proj_fixed_target == FALSE) {
      stands <- stands %>%
        set_variable_target(
          group_by = proj_id_field,
          target_field = proj_target_field,
          multiplier = proj_target_value
        )
    }
  }
  return(stands)
}

#' TODO
#' @param stands TODO
#' @param target_value TODO
#' @return TODO
#'
#' @importFrom data.table :=
#'
set_fixed_target <- function(stands, target_value) {
  stands[, master_target := {{ target_value }}]
}

#' TODO
#' @param stands TODO
#' @param group_by TODO
#' @param target_field TODO
#' @param multiplier TODO
#' @return TODO
#'
#' @importFrom data.table :=
#'
set_variable_target <- function(stands, group_by, target_field, multiplier){
  stands[, ':='(proj_target, sum(get(target_field))), by = list(get(group_by))]
  stands[, master_target :=  proj_target * multiplier]
}

#' Select stands for treatment based on project stand thresholds and
#' threshold/targets
#'
#' @param stands TODO
#' @param stand_id_field TODO
#' @param proj_id_field TODO
#' @param proj_objective TODO
#' @param proj_target_field TODO
#' @param proj_target TODO
#'
#' @return TODO
#'
#' @importFrom rlang .data
#' @importFrom dplyr mutate arrange
#'
apply_treatment <- function(
    stands,
    stand_id_field,
    proj_id_field,
    proj_objective = 'weightedPriority',
    proj_target_field,
    proj_target = 'master_target',
    treatment_name = NULL
) {
  
  # select stands for treatment
  stands_treated <- stands %>%
    select_simple_greedy_algorithm(
      grouped_by = proj_id_field,
      prioritize_by = proj_objective,
      constrain_by = c(1, proj_target_field, proj_target)) %>%
    mutate(treatment_name = !!treatment_name)
  
  # update the total area available for activities.
  proj_objective_treated <- stands_treated %>%
    create_grouped_dataset(
      grouping_vars = proj_id_field,
      summing_vars = c(proj_target_field, 'weightedPriority')) %>%
    arrange(-.data$weightedPriority)
  
  return(stands_treated)
}

#' Select subunits to treat based on a given priority.
#'
#' @param dt A data table with all the subunits and attributes.
#' @param grouped_by The management objective that is being prioritized.
#' @param prioritize_by The stand value for the priority that is used to
#'   constrain the results. Typically Area or Volume.
#' @param constrain_by This is a list of lists that includes three fields for
#'   each constraint. The first item in each list, default 'apply', is the name
#'   of a binary field in which 1 = apply the constraint and 0 = do not apply
#'   the constraint to a given stand. The second item in each list is the name
#'   of the field that is summed to reach the constraint. The third item in each
#'   list is the name of the constraining field.
#' @return The selected stands from \code{df}, ordered by \code{prioritize_by},
#'   and selected until the sum of \code{tally_by} is as close to
#'   \code{group_target} as possible.
#'
#' @importFrom data.table := setDT

select_simple_greedy_algorithm <- function(dt, grouped_by, prioritize_by, constrain_by) {
  # For each grouped_by: Remove subunits that don't meet threshold; Order by
  # priority; Select stands up to treatment target; The operator in the line
  # below determines the direction and whether the threshold is inclusive or
  # not.
  dt <- setDT(dt)
  
  # define default values
  dt[, considerForTreatment := 1]
  dt[, current_target := 0]
  dt[, cumulative_tally_by := 0]
  dt[, selected := 0]
  
  # order by group and descending priority
  dt <- dt[order(dt[,get(grouped_by)], -dt[,get(prioritize_by)])]

  # Common issue: if the dataset has na values in the priority field, this will fail.
  while(sum(dt[,considerForTreatment==1 & selected == 0], na.rm=T) != 0){
    # Order the data table by the priority.
    dt <- dt[order(dt[,get(grouped_by)], -dt[,get(prioritize_by)])]
    # Determine the current target
    dt[, current_target := 0]
    # Sum the treated tally by group
    dt[selected == 1, current_target := sum(get(constrain_by[2]), na.rm=T), by=list(get(grouped_by))]
    # The current target is the target value less the value of the already treated stands, by group.
    dt[, current_target := get(constrain_by[3]) - max(current_target, na.rm=T), by=list(get(grouped_by))]
    # Compute the cumulative sum for the constrained variable and select subunits
    dt[considerForTreatment == 1 & selected == 0, cumulative_tally_by := cumsum(get(constrain_by[2])), by=list(get(grouped_by))]
    dt[considerForTreatment == 1 & selected == 0 & cumulative_tally_by <= current_target, selected := 1 ]
    # Determine the remaining target
    dt[, current_target := 0]
    dt[selected == 1, current_target := sum(get(constrain_by[2]), na.rm=T), by=list(get(grouped_by))]
    dt[, current_target := get(constrain_by[3]) - max(current_target, na.rm=T), by=list(get(grouped_by))]
    # Remove from treatment consideration if subunit value is greater than total target.
    dt[considerForTreatment == 1 & selected == 0, considerForTreatment := ifelse(get(constrain_by[2]) > current_target, 0, 1 )]
  }
  dt <- dt[selected == 1,]
  return(dt)
}


#' Weight priorities for selection
#'
#' @param numPriorities TODO
#' @param weights TODO
#' @return A datatable with the weighted values for the priorities in the \code{priorityList}.
#'
weight_priorities <- function(numPriorities, weights = c("1 1 1")){
  if(numPriorities == 1)
    return(data.table::data.table(1))
  weights <- strtoi(unlist(strsplit(weights, " ")))
  weights <- seq(weights[1], weights[2], weights[3])
  # Updates by Luke Wilkerson to incorporate multiple priorities.
  weightPermute <- (gtools::permutations(length(weights), numPriorities, weights, repeats.allowed=TRUE))
  weightprops <- proportions(weightPermute, 1)
  weightPermute <- data.table::data.table(weightPermute)
  uniqueWeightCombinations <- weightPermute[!duplicated(weightprops) & rowSums(weightPermute) != 0, ]

  return(uniqueWeightCombinations)
}

# Hack the area target - can be set in the shapefile. This code may be updated
# for looping multiple treatment types. It should do the same thing that Pedro
# is working on within a single run instead of wrapped.

set_up_treatment_types <- function(stands, args=NULL) {
  stands$selected <- 0
  stands$proj_target_treated <- 0
  stands$weightedPriority <- 0
  return(stands)
}


#' Title
#'
#' @param stands 
#' @param w 
#' @param priorities 
#' @param weights 
#' 
#' @importFrom data.table :=
#'
#' @return

set_up_priorities <- function(stands, w, priorities, weights) {
  for (i in 1:ncol(weights)) {
    curr_weight = weights[[i]][w]
    curr_priority = priorities[[i]][1]
    stands$weightedPriority <- stands$weightedPriority + curr_weight * stands[,get(curr_priority)]
    priorityName <- paste0("Pr_", i, "_", curr_priority)
    stands[, (priorityName) := curr_weight]
  }
  return(stands)
}

