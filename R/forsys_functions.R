#' Wrapper for building projects using predetermined project areas
#'
#' @param stands Data frame containing stand data
#' @param stand_id_field Field name containing unique stand id.  <emph{character}>
#' @param stand_area_field Field name containing stand area.  <emph{character}>
#' @param proj_id_field Field name containing unique project area id.  <emph{character}>
#' @param proj_target_field Field name used as primary constraint.  <emph{character}>
#' @param proj_fixed_target Whether project target is fixed or relative. <emph{logical}>
#' @param proj_target_value Absolute value or relative percent (0-1) of project area sum.
#' @param proj_target_min_value TODO ???
#' @param stand_threshold Boolean statement on stand availability for treatment. <emph{character}>
#' @param proj_number  Number of projects. <emph{Numeric}>
#' @param global_ceiling_field Ceiling field containing values. <emph{character}>
#' @param global_ceiling Ceiling not be to exceeded across all projects.  <emph{Numeric}>
#'
#' @importFrom dplyr rename select mutate arrange left_join inner_join
#' @importFrom tidyr drop_na
#'
build_preset_projects <- function(
    stands,
    stand_id_field,
    stand_area_field,
    proj_id_field,
    proj_target_field,
    proj_fixed_target,
    proj_target_value,
    proj_target_min_value = -Inf,
    stand_threshold,
    proj_number = NULL,
    global_ceiling_field = NULL,
    global_ceiling_value = Inf
){
  
  # define global variables
  weightedPriority <- treatment_rank <- DoTreat <- NULL
  
  # stand selection based on predetermined projects
  stands <- stands %>%
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
    select(stand_id_field, proj_id_field, stand_area_field, global_ceiling_field, 'weightedPriority') %>%
    mutate(DoTreat = 1)
  
  # group selected stands by project, summarize, and rank
  projects_selected <- stands_selected %>%
    create_grouped_dataset(
      grouping_vars = proj_id_field,
      summing_vars = c(stand_area_field, global_ceiling_field, 'weightedPriority')) %>%
    replace(is.na(.), 0) %>%
    arrange(-weightedPriority) %>%
    mutate(treatment_rank = ifelse(weightedPriority > 0, 1:dplyr::n(), NA)) %>%
    drop_na(treatment_rank)
  
  # filter project level output by project number or treatment area ceiling
  proj_number = ifelse(is.null(proj_number), Inf, proj_number)
  global_ceiling_value = ifelse(global_ceiling_value %>% is.na, Inf, global_ceiling_value)
  projects_selected_out <- projects_selected %>%
    filter(treatment_rank <= proj_number)
  
  # filter by global ceiling if field is specified
  if(!is.null(global_ceiling_field)){
    projects_selected_out <- projects_selected_out %>%
      filter(get(global_ceiling_field) <= global_ceiling_value)
  }
    
  # create stand level output by joining with output projects
  join_y = stands_selected %>% 
    inner_join(
      projects_selected_out %>% select(proj_id_field, 'treatment_rank'), 
      by = proj_id_field) %>% 
    select(stand_id_field, DoTreat)
  
  stands_selected_out <- stands %>%
    select(stand_id_field, proj_id_field, stand_area_field, 'weightedPriority') %>%
    left_join(join_y, stand_id_field) %>%
    mutate(DoTreat = ifelse(DoTreat %>% is.na, 0, 1))
  
  stands_selected_out <- stands_selected_out %>%
    inner_join(
      projects_selected_out %>% select(proj_id_field, 'treatment_rank'), 
      by = proj_id_field) %>%
    arrange(treatment_rank, stand_id_field)
  
  stands_selected_out <- stands_selected_out %>%
    mutate(DoTreat = 1, selected = 1)
  
  # limit output to project ceiling if set
  if(!is.null(global_ceiling_field)) {
    
    projects_selected_out <- projects_selected_out %>%
      filter((cumsum(get(global_ceiling_field)) %/% global_ceiling_value + 1) == 1)

    stands_selected_out <- stands_selected_out %>%
      filter(get(proj_id_field) %in% unique(projects_selected_out %>% pull(!!proj_id_field)))
  }
  
  return(list(
    projects_selected_out,
    stands_selected_out
  ))
}



#' ForSysR wrapper for running patchmax
#'
#' @return list
build_dynamic_projects <- function(
    stands, 
    proj_target_field, 
    stand_id_field, 
    proj_id_field,
    stand_area_field, 
    stand_threshold,
    patchmax_proj_size, 
    patchmax_proj_size_min,
    patchmax_proj_number,
    proj_target_value, 
    proj_target_min_value, 
    global_ceiling_field,
    global_ceiling_value,
    patchmax_SDW, 
    patchmax_EPW, 
    patchmax_exclusion_limit,
    patchmax_sample_frac, 
    patchmax_sample_seed,
    patchmax_verbose
  ){
  
  geom <- sf::st_as_sf(stands)
  
  if (!is.null(proj_target_field)) { 
    P_constraint = pull(geom, !!proj_target_field)
  } else {
    P_constraint = NULL
    proj_target_value = Inf
  }
  
  # modified pull functional when field is NULL
  pull_field <- function(data, field) {
    if (is.null(field)) {
      out <- NULL
    }
    else {
      out <- pull(data, !!field)
    }
  }
  
  patchmax_out <- patchmax::simulate_projects(
    geom = geom,
    St_id = pull_field(geom, stand_id_field), 
    St_area = pull_field(geom, stand_area_field), 
    St_objective = pull_field(geom, 'weightedPriority'), 
    St_threshold = stand_threshold,
    P_size = patchmax_proj_size, 
    P_size_min  = patchmax_proj_size_min, 
    P_number = patchmax_proj_number,
    P_ceiling = pull_field(geom, global_ceiling_field),
    P_ceiling_max = global_ceiling_value,
    SDW = patchmax_SDW,
    EPW = patchmax_EPW,
    exclusion_limit = patchmax_exclusion_limit,
    P_constraint = P_constraint,
    P_constraint_max_value = proj_target_value,
    P_constraint_min_value = proj_target_min_value,
    sample_frac = patchmax_sample_frac,
    sample_seed = patchmax_sample_seed,
    verbose = patchmax_verbose
  )
  
  projects_selected <- patchmax_out[[1]] %>%
    rename(treatment_rank = Project, weightedPriority = Objective) %>%
    mutate(!!proj_id_field := treatment_rank)
  
  stands_selected <- patchmax_out[[2]] %>%
    select(!!stand_id_field := Stands,
           !!proj_id_field := Project,
           treatment_rank := Project,
           DoTreat := DoTreat,
           weightedPriority = Objective) %>%
    mutate(selected = 1)
  
  return(list(
    projects_selected, 
    stands_selected
    ))
} 

#' Threshold string statement parser
#'
#' @param stands Dataframe containing stand data
#' @param proj_id_field Field name containing project id
#' @param proj_fixed_target Logical whether target is fixed or relative
#' @param proj_target_field Field name containing target value to be summed
#' @param proj_target_value Numeric or percent (0 - 1) depending on `proj_fixed_target`
#'
set_treatment_target <- function(
    stands,
    proj_id_field,
    proj_fixed_target,
    proj_target_field=NULL,
    proj_target_value=NULL
) {
  
  if(length(proj_fixed_target > 0)){
    
    # target based on fixed total
    if (proj_fixed_target == TRUE) {
      stands <- stands %>%
        mutate(master_target = proj_target_value)
    } 
    
    # target based on percent total of field
    if (proj_fixed_target == FALSE) {
      setDT(stands)
      stands[, ':='(proj_target, sum(get(proj_target_field))), by = list(get(proj_id_field))]
      stands[, master_target :=  proj_target * proj_target_value]
      setDF(stands)
    }
  }
  return(stands)
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
#' @param treatment_name TODO
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
    select_stands_by_group(
      grouped_by = proj_id_field,
      prioritize_by = proj_objective,
      constrain_by = proj_target_field,
      constraint_limit = proj_target) %>%
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
#' @param dt data with all the subunits and attributes.
#' @param grouped_by field name with project or planning area ids 
#' @param prioritize_by field name of priority to be maximized
#' @param constrain_by field name of constraint (e.g., are)
#' @param constraint_limit field name containing cumulative constraint not to be exceeded
#' 
#' @return The selected stands from \code{df}, ordered by \code{prioritize_by},
#'   and selected until the sum of \code{tally_by} is as close to
#'   \code{group_target} as possible.
#'
#' @importFrom data.table := setDT setDF
#'
select_stands_by_group <- function(
    dt, 
    grouped_by, 
    prioritize_by, 
    constrain_by,
    constraint_limit) {
  
  setDT(dt)
  
  # assign default values for required fields
  dt[, considerForTreatment := 1]
  dt[, current_target := 0]
  dt[, cumulative_tally_by := 0]
  dt[, selected := 0]
  
  # order by group and descending priority
  dt <- dt[order(dt[, get(grouped_by)], -dt[, get(prioritize_by)])]

  # while unselected stands consider for treatment remain...
  while(sum(dt[,considerForTreatment == 1 & selected == 0], na.rm=T) != 0){
    
    # order stands by priority within groups
    dt <- dt[order(dt[,get(grouped_by)], -dt[,get(prioritize_by)])]
    
    # set current target to 0s
    dt[, current_target := 0]
    
    # sum selected stands by group
    dt[selected == 1, current_target := sum(get(constrain_by), na.rm=T), by = list(get(grouped_by))]
    
    # within each group, set current target to constraint target minus current target
    dt[, current_target := get(constraint_limit) - max(current_target, na.rm=T), by=list(get(grouped_by))]
    
    # calculate the cumulative sum for the constrained variable for considered by unselected stands
    dt[considerForTreatment == 1 & selected == 0, cumulative_tally_by := cumsum(get(constrain_by)), by=list(get(grouped_by))]
    
    # select stands for unselected stands where the cumulative tally is less than the current target
    dt[considerForTreatment == 1 & selected == 0 & cumulative_tally_by <= current_target, selected := 1]
   
    # recalculate the current target within each group
    dt[selected == 1, current_target := sum(get(constrain_by), na.rm=T), by=list(get(grouped_by))]
    
    # calculate current target
    dt[, current_target := get(constraint_limit) - max(current_target, na.rm=T), by=list(get(grouped_by))]
    
    # set consideration to 0 constraint is greater than the current target
    dt[considerForTreatment == 1 & selected == 0, considerForTreatment := ifelse(get(constrain_by) > current_target, 0, 1 )]
  }
  
  # select and return selected stands
  dt_out <- dt[selected == 1,]
  
  return(setDF(dt_out))
}


#' Weight priorities for selection
#'
#' @param numPriorities TODO
#' @param weights TODO
#' @return A datatable with the weighted values for the priorities in the \code{priorityList}.
#' 
#' @importFrom gtools permutations 
#'
weight_priorities <- function(numPriorities, weights = c(1, 1, 1)){
  
  if(numPriorities == 1){
    return(data.frame(1))
  }
  
  # process weights string
  if(is.character(weights)){
    weights <- strtoi(unlist(strsplit(weights, " ")))
  }
  weights <- seq(weights[1], weights[2], weights[3])
  
  # Updates by Luke Wilkerson to incorporate multiple priorities.
  weightPermute <- permutations(length(weights), numPriorities, weights, repeats.allowed=TRUE)
  weightprops <- proportions(weightPermute, 1)
  weightPermute <- data.frame(weightPermute)
  uniqueWeightCombinations <- weightPermute[!duplicated(weightprops) & rowSums(weightPermute) != 0, ]

  return(uniqueWeightCombinations)
  
  # # DRAFT FUNCTION FOR PERMUTING WEIGHTS
  # func <- function(x, s=3) log10(x/(1-x))/s
  # 
  # # plot differences in S
  # num_seq <- seq(0,1,.01)
  # plot(func(num_seq, s=2), type='l', col='red')
  # points(func(num_seq, s=1.33), type='l', col='green')
  # points(func(num_seq, s=5), type='l', col='blue')
}

#' Create a dataset by subsetting subunits that were selected in the
#' selectSubunits function and grouping the data by a larger subunit (usually
#' planning areas).
#'
#' @param data data with all the subunits and attributes.
#' @param grouping_vars The variable names by which the data will be grouped.
#' @param summing_vars The variables in the original dataset that need to be
#'   summed over each subunit.
#' @return The selected stands from \code{df}, ordered by \code{priority_SPM},
#'   and selected until the sum of \code{priority_STND} is as close to
#'   \code{treat_target} as possible.
#'  
#' @importFrom dplyr group_by_at vars summarize_at

create_grouped_dataset <- function(data, grouping_vars, summing_vars) {
  
  grouping_vars <- unique(grouping_vars)
  summing_vars <- unique(summing_vars)
  
  data <- data %>% 
    group_by_at(vars(grouping_vars)) %>%
    summarize_at(vars(summing_vars), sum)
  
  data <- data %>% ungroup()
  
  return(data)
}

#' Summarize project data
#'
#' @param selected_stands 
#' @param stands_data 
#' @param stand_id_field 
#' @param stand_area_field character. Name of variable containnig the stand area
#' @param proj_id_field 
#' @param scenario_output_grouping_fields 
#' @param scenario_output_fields 
#'
#' @return list containing summary for selected projects and stands
#' @export

summarize_projects <- function(
    selected_stands,
    stands_data,
    stand_id_field,
    stand_area_field,
    proj_id_field,
    scenario_output_grouping_fields,
    scenario_output_fields
){
  
  # append specified output attributes to selected stands
  flds <- c(scenario_output_grouping_fields, scenario_output_fields)
  join_x <- selected_stands %>% select(!!stand_id_field, !!proj_id_field, DoTreat, ETrt_YR)
  join_y <- stands_data %>% select(!!stand_id_field, any_of(flds), weightedPriority)
  selected_stands_plus <- left_join(join_x, join_y, by = stand_id_field, suffix = c("", "..dup")) %>%
    select(!contains('..dup')) 
  
  # summarize by project & year and tag with ETrt_ prefix
  project_sum <- create_grouped_dataset(
    data = selected_stands_plus %>% filter(DoTreat == 1),
    grouping_vars = c(proj_id_field, 'ETrt_YR'),
    summing_vars = scenario_output_fields)
  
  total_coverage <- create_grouped_dataset(
    data = selected_stands_plus,
    grouping_vars = c(proj_id_field, 'ETrt_YR'),
    summing_vars = stand_area_field) %>%
    pull(stand_area_field)
  
  project_sum <- project_sum %>% 
    mutate(pct_excluded = 1 - (get(stand_area_field) / total_coverage)) %>%
    arrange(-weightedPriority) %>%
    mutate(treatment_rank = rank(-weightedPriority, ties.method = 'first'))
  
  subset_sum <- create_grouped_dataset(
    data = selected_stands_plus %>% filter(DoTreat == 1),
    grouping_vars = c(proj_id_field, 'ETrt_YR', scenario_output_grouping_fields),
    summing_vars = scenario_output_fields) %>%
    left_join(project_sum %>% select(!!proj_id_field, treatment_rank), by = proj_id_field) %>%
    arrange(treatment_rank)
  
  out <- list(
    projects = project_sum %>% 
      rename_with(.fn = ~ paste0("ETrt_", .x), .cols = scenario_output_fields),
    subset = subset_sum %>% 
      rename_with(.fn = ~ paste0("ETrt_", .x), .cols = scenario_output_fields)
  )
  
  return(out)
}

#' Combine priorities
#'
#' Combines 2 or more priorities into a new field.
#'
#' @param stands Data frame containing stand data
#' @param fields Field names (2 or more) to combine
#' @param weights Numeric vector with weights. Assume equal weighting if NULL
#' @param new_field Name to assign combined priority
#' 
#' @importFrom glue glue
#' @importFrom sf st_drop_geometry
#' @importFrom dplyr select all_of mutate
#' @export
#' 
combine_priorities <- function(
    stands, 
    fields = NULL, 
    weights = NULL, 
    new_field = 'combined_priority',
    append_weights = FALSE
) {
  
  if (is.null(weights) | length(weights) == 1) {
    weights = rep(1, length(fields))
  }
  
  if (is.null(fields) | length(fields) != length(weights)) {
    stop('Function requires >= 2 fields and a vector with weight values of equal length ')
  }
  
  sp <- stands %>% st_drop_geometry() %>% select(all_of(fields))
  cp <- apply(t(sp) * weights, 2, sum)
  stands <- stands %>% mutate(!!new_field := cp)
  message(glue::glue('Combined priority assigned to ', new_field))
  
  if (append_weights) {
    for(i in 1:length(fields)) {
      weight_i <- weights[i]
      name_i <- paste0("Pr_", i , "_", fields[i])
      stands <- stands %>% mutate(!!name_i := weight_i)
    }
  }
  
  return(stands)
}


#' Filter data
#'
#' Used to filter stands to a specific criteria, often because certain stands
#' are considered outside the study area or otherwise excluded (e.g., wilderness
#' area, private lands, etc.)
#'
#' @param stands Data table to filter
#' @param filter_txt Boolean statement as character string
#' @param verbose Boolean statement to report filtered results
#'
#' @importFrom rlang .data
#' @import glue
#' @export
#' 
filter_stands <- function(stands, filter_txt = NULL, drop = FALSE, verbose = TRUE) {
  if (is.null(filter_txt)) {
    return(stands)
  }
  tryCatch({
    out <- NULL # helps devtools::check()
    out <- subset(stands, eval(parse(text = filter_txt)))
    n0 <- nrow(stands)
    n1 <- nrow(out)
    if (verbose)
      message(glue("Subsetting stands where {filter_txt} ({round((n0-n1)/n0*100,2)}% excluded)"))
  }, error = function(e) {
    message(paste0('!! Filter failed; proceeding with unfiltered data. Error message:\n', print(e)))
  })
  return(out)
}


# ---------------------------

shuffle_projects <- function(projects){
  message('!! Randomizing projects')
  
  shuffled_weights <- projects_selected_y %>%
    filter(treatment_rank %>% is.na == FALSE) %>%
    mutate(weightedPriority = sample(weightedPriority)) %>%
    select(proj_id_field, weightedPriority)
  
  projects_selected_y <- projects_selected_y %>% select(-weightedPriority) %>%
    left_join(shuffled_weights, by = proj_id_field) %>%
    arrange(-weightedPriority) %>%
    mutate(treatment_rank = ifelse(weightedPriority > 0, 1:n(), NA)) %>%
    tidyr::drop_na(treatment_rank)
  
  return(projects_selected_y)
}

