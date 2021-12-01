load_R_config <- function(config_file){
  source(config_file, local = TRUE)
  for(i in ls()){
    exists(i, envir = parent.frame)
  }
}
#' Load stored in JSON config into the current enviroment
#'
#' @param json_filename character string of forsys json config

load_json_config <- function(json_filename){
  json_data = readLines(json_filename) %>%
    jsonlite::fromJSON()
  for(i in names(json_data)){
    assign(i, json_data[[i]], env = parent.frame())
  }
}

#' Load the input dataset. Supports both CSV and DBF.
#'
#' @param path_to_file Path to an input dataset
#' @return Loaded data.table from the input dataset
#' @export
#'
load_dataset <- function(path_to_file) {
  file_type <- stringr::str_sub(path_to_file, start= -3)
  message("Loading Dataset")
  if (file_type == "dbf") {
    standDT <- data.table(foreign::read.dbf(path_to_file))
    message("Read stand file from DBF")
  } else if (file_type == "csv") {
    standDT <- data.table(data.table::fread(path_to_file, header = TRUE))
    message("Read stand file from CSV")
  } else {
    message('Input format not recognized')
  }
  return(standDT)
}

#' Select subunits to treat based on a given priority.
#'
#' @param dt A data table with all the subunits and attributes.
#' @param grouped_by The management objective that is being prioritized.
#' @param prioritize_by The stand value for the priority that is used to constrain the results.
#' Typically Area or Volume.
#' @param constrain_by This is a list of lists that includes three fields for each constraint. The
#' first item in each list, default 'apply', is the name of a binary field in which 1 = apply the constraint
#' and 0 = do not apply the constraint to a given stand. The second item in each list is the name
#' of the field that is summed to reach the constraint. The third item in each list is the name of
#' the constraining field.
#' @return The selected stands from \code{df}, ordered by \code{prioritize_by}, and selected until the sum of \code{tally_by} is as close to
#' \code{group_target} as possible.
#'
#' @importFrom data.table :=
#'
select_simple_greedy_algorithm <- function(dt = NULL,
                                           grouped_by = 'PA_ID',
                                           prioritize_by = 'TVMBF_SPM',
                                           constrain_by = c('apply', 'AREA_HA', 'AREA_PA10P')) {
  # For each grouped_by:
  # Remove subunits that don't meet threshold:
  # Order by priority:
  # Select stands up to treatment target
  # The operator in the line below determines the directionality and whether the threshold is inclusive or not.
  dt <- data.table(dt)
  dt[, considerForTreatment := 1]
  dt[, current_target := 0]
  dt[, cumulative_tally_by := 0]
  dt[, selected := 0]
  dt <- dt[order(dt[,get(grouped_by)], -dt[,get(prioritize_by)])]
  #dt <- dt[, activity_code := do_treat(c(.BY, .SD), constrain_by[2]), by = "PA_ID"]
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

#' Produce a set of stands that can be treated under a given criteria.
#' @param dt A data table with all stand information necessary to determine availability for a specific treatment type.
#' @param filters A list of strings that are used to filter the stands for treatment availability.
#' @return The final data table with stands available for treatment.
#'
stand_filter <- function(dt, filters) {
  for(f in 1:nrow(filters)){
    filter <- paste0(filters[f,2], " ", filters[f,3], " ", filters[f,4])
    filter <- stringr::str_remove(filter, "'")
    filter <- stringr::str_remove(filter, "'")
    dt <- subset(dt, eval(parse(text = filter)))
  }
  ## Hard-coded manageable/undisturbed.
  #dt <- subset(dt, man_alldis == 1)
  return(dt)
}

#' Create a new field in a stand table that flags all stands that include a given set of criteria
#'
#' @param dt A data table with all stand information necessary to determine availability for a specific treatment type.
#' @param filters A list of strings that are used to filter the stands for treatment availability.
#' @param field The name of a new field
#' @return The final data table with stands available for treatment.
#'
#' @importFrom data.table :=
#'
stand_flag <- function(dt, filters, field) {
  dt[, (field) := 0]
  for(f in 1:nrow(filters)){
    filter <- paste0(filters[f,2], " ", filters[f,3], " ", filters[f,4])
    filter <- stringr::str_remove(filter, "'")
    filter <- stringr::str_remove(filter, "'")
    dt <- dt[ eval(parse(text = filter)), (field) := 1]
  }
  return(dt)
}

#' Create a dataset by subsetting subunits that were selected in the selectSubunits function
#' and grouping the data by a larger subunit (usually planning areas).
#'
#' @param dt A data table with all the subunits and attributes.
#' @param grouping_vars The variable names by which the data will be grouped.
#' @param summing_vars The variables in the original dataset that need to be summed over each subunit.
#' @param subset_var TODO
#' @return The selected stands from \code{df}, ordered by \code{priority_SPM}, and selected until the sum of \code{priority_STND} is as close to
#' \code{treat_target} as possible.
#'
create_grouped_dataset <- function(dt,
                                 grouping_vars,
                                 summing_vars,
                                 subset_var = NULL) {
  ## Create the grouped data.table by grouping the treated subunits from the previous step.
  if(!is.null(subset_var)){
    dt <- subset(dt[get(subset_var)==1])
  }
  dt <- dplyr::group_by_at(dt, dplyr::vars(grouping_vars))
  dt <- data.table::data.table(dplyr::summarize_at(dt, .vars = dplyr::vars(summing_vars), .funs = c(sum="sum")))
  names(dt) <- gsub(x = names(dt), pattern = "_sum", replacement = "")
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


#' Update target area for treatment after each activity
#'
#' @param treated_stands TODO
#' @param subunit TODO
#' @param unit_area TODO
#' @return A table with the updated subunit targets for all planning areas that had treatments
#'
update_target <-function(treated_stands, subunit, unit_area) {
  treated_subunit_target <- create_grouped_dataset(treated_stands,
                                               subunit,
                                               unit_area)
}

printSpecsDocument <- function(subunit, priorities, timber_threshold, volume_constraint) {
  parameters <- paste0("ForSys simulation designed and coded by Rachel Houtman, run on: ", Sys.Date(), "\n",
                       "This code creates attainment graphs for to explore both WUI exposure (HUIDW) and merchantable timber volume\n",
                       "with the following ForSys settings:\n",
                       "1) Subunit = PA_ID_New\n",
                       "2) treated area = 15% of manageable planning area, field = man_alldis\n",
                       "3) project number = NA\n",
                       "4) prioritize by potential merchantable volume AND WUI exposure\n",
                       "5) Timber threshold > 0\n",
                       "6) No forest volume constraint\n",
                       "7) Running on ForSys\n",
                       "\n",
                       "Required files:\n",
                       "1) ForSys output - proj_all.csv")
  writeLines(parameters, file("README.txt"))

}

#' Filter data
#'
#' @param stands Data table to filter
#' @param filter_txt Boolean statement as character string
#' @param verbose Boolean statement to report filtered results
#'
filter_stands <- function(stands, filter_txt, verbose = TRUE){
  tryCatch({
    eval_txt <- paste0("out <- stands[", filter_txt ,"]")
    eval(parse(text=eval_txt))
    n0 <- nrow(stands)
    n1 <- nrow(out)
    if(verbose)
      message(glue::glue("----------\nFiltering stands where: {filter_txt} ({round((n0-n1)/n0*100,2)}% excluded)\n-----------"))
  }, error = function(e){
    message(paste0('!! Filter failed; proceeding with unfiltered data. Error message:\n', print(e)))
  })
  return(out)
}

#' Add spm and pcp values for specified fields
#'
#' @param stands data.table of stands
#' @param fields vector of character field names to calculate pcm & spm values
#'
calculate_spm_pcp <- function(stands, fields){
  for (f in fields) {
    maximum <- max(stands[, get(f)], na.rm=T)
    cn <- paste0(f, "_SPM")
    expr <- bquote(.(as.name(cn)):= 0)
    stands[,eval(expr)]
    expr <- bquote(.(as.name(cn)):= (100 * get(f) / maximum))
    stands[, eval(expr)]

    sum.total <- sum(as.numeric(stands[, get(f)]), na.rm=T)
    cn <- paste0(f, "_PCP")
    expr <- bquote(.(as.name(cn)):= 0)
    stands[,eval(expr)]
    expr <- bquote(.(as.name(cn)):= (100 * get(f) / sum.total))
    stands[, eval(expr)]
  }
  return(stands)
}

# Hack the area target - can be set in the shapefile.
# This code may be updated for looping multiple treatment types.
# It should do the same thing that Pedro is working on within a single run instead of wrapped.
set_up_treatment_types <- function(stands, args=NULL) {
  stands$Commercial <- stands$Commercial*stands$AREA_HA
  stands$PreCommercial <- stands$AREA_HA - stands$Commercial
  stands$selected <- 0
  stands$treatment_type <- ""
  stands$treatedPAArea <- 0
  stands$weightedPriority <- 0
  stands$treat <- 0
  return(stands)
}

#' TODO
#' @param stands TODO
#' @param i TODO
#' @param weight TODO
#' @param priority TODO
#' @return TODO
#'
#' @importFrom data.table :=
#'
set_up_priorities_helper <- function(stands, i, weight, priority) {
  stands$weightedPriority <- stands$weightedPriority + weight * stands[, get(priority)]
  priorityName <- paste0("Pr_", i, "_", priority)
  stands[, (priorityName) := weight]
}

set_up_priorities <- function(stands, w, priorities, weights) {

  for (i in 1:ncol(weights)) {
    curr_weight = weights[[i]][w]
    curr_priority = priorities[[i]][1]
    stands <- set_up_priorities_helper(stands, i, curr_weight, curr_priority)
  }
  return(stands)
}

#' Threshold string statement parser
#' @param thresholds Vector of Boolean string statements to parse

make_thresholds <- function(thresholds) {
  # txt <- 'RxFire: FRG %in% 1:3 & Manage == 0; RxReburn: RxFire == 1'
  txt <- thresholds
  if(stringr::str_detect(txt, ':')==FALSE){
    txt <- stringr::str_replace(txt, ' ', ': ')
    warning(glue::glue('!! Rx threshold statement "{txt}" missing a colon. Assuming string before first space is the treatment name.'))
  }
  out <- txt %>%
    stringr::str_replace_all(' ','') %>%
    stringr::str_split(';', simplify = T) %>%
    stringr::str_split(':', simplify = T, n=2) %>%
    as.data.frame() %>%
    rename(type = 1, threshold = 2)
  return(out)
}

#' TODO
#' @param stands TODO
#' @param treatment_type TODO
#' @param treatment_threshold TODO
#' @param stand_id_field TODO
#' @param proj_id TODO
#' @param proj_fixed_target TODO
#' @param proj_fixed_area_target TODO
#' @param proj_target_field TODO
#' @param proj_variable_target_multiplier TODO
#' @return TODO
#'
#'
apply_treatment <- function(stands,
                            treatment_type,
                            treatment_threshold,
                            stand_id_field,
                            proj_id,
                            proj_fixed_target,
                            proj_target_field=NULL,
                            proj_target_value=NULL
                            # proj_fixed_target_value=NULL,
                            # proj_variable_target_multiplier=NULL
                            ) {
  stands_updated <- stands
  selected_stands <- NULL

  # for each treatment type
  for (t in 1:length(treatment_type)) {

    # filter stands by threshold type criteria
    filtered_stands <- stands %>% filter_stands(treatment_threshold[t], verbose = F)

    # set project target
    if (proj_fixed_target == TRUE) {
      # target based on fixed total
      filtered_stands <- filtered_stands %>%
        set_fixed_target(target_value = proj_target_value)
    } else if (proj_fixed_target == FALSE) {
      # target based on percent total of field
      filtered_stands <- filtered_stands %>%
        set_variable_target(group_by = proj_id,
                            target_field = proj_target_field,
                            multiplier = proj_target_value)
    }

    # select stands for current treatment type
    treat_stands <- select_simple_greedy_algorithm(dt = filtered_stands,
                                                   grouped_by = proj_id,
                                                   prioritize_by = "weightedPriority",
                                                   constrain_by = c(1, proj_target_field, "master_target"))

    # This updates the total area available for activities. Original treatment target - total area treated for each subunit (planning area).
    area_treatedPA <- update_target(treat_stands, proj_id, proj_target_field)
    stands_updated <- stands_updated[area_treatedPA,  treatedPAArea := treatedPAArea + i.sum, on = proj_id]
    stands_updated <- stands_updated[treat_stands, ':='(treatment_type = treatment_type[t], selected = 1), on = stand_id_field]
    selected_stands <- rbind(selected_stands, stands_updated[selected==1,])
  }

  return(selected_stands)
}

#' TODO
#' @param stands TODO
#' @param fixed_area_target TODO
#' @return TODO
#'
#' @importFrom data.table :=
#'
set_fixed_target <- function(stands, target_value) {
  # TODO this {{ }} probably doesn't work
  stands[, master_target := {{ target_value }}]
}

#' TODO
#' @param stands TODO
#' @param proj_target_field TODO
#' @param proj_variable_target_multiplier TODO
#' @return TODO
#'
#' @importFrom data.table :=
#'
set_variable_target <- function(stands, group_by, target_field, multiplier){
    stands[, ':='(proj_target, sum(get(target_field))), by = list(get(group_by))]
    stands[, master_target :=  proj_target * multiplier]
}

# #' TODO
# #' @param grouped_by_pa TODO
# #' @return TODO
# #'
# #' @importFrom data.table :=
# #'
# identify_nested_planning_areas <- function(grouped_by_pa) {

#   # Step 3: Identify the best planning areas within each nest.
#   message("Selecting planning area subunits")
#   groupedByPA$system <- 1


#   grouping_type <- nesting_unit
#   groupedByPA[,harvestTarg := groupedByPA[,get(nesting_target) * get(nesting_unit)]]

#   groupedByPA$selected <- 0
#   paSubunits <- select_simple_greedy_algorithm(dt = groupedByPA,
#                                             grouped_by = grouping_type,
#                                             prioritize_by = "weightedPriority",
#                                             grouped_target = "harvestTarg")
#   paSubunits <- paSubunits[order(-paSubunits$weightedPriority),]
#   message("adding treatment rank")
#   paSubunits$treatment_rank <- seq(1:nrow(paSubunits))
# }


#' Write individual stand output to file
#' TODO this should go in _results
#'
#' @param selected_stands TODO
#' @param dir TODO
#' @param name TODO
#' @param write_fields TODO
#' @return
#'
#' @importFrom dplyr %>%
#'
write_stand_outputs_to_file <- function(selected_stands, dir, name, write_fields) {
    stand_output_file <- paste0(dir, "/stnd_",  name, ".csv")
    data.table::fwrite(selected_stands %>% dplyr::select(write_fields), stand_output_file)
}

#' TODO
#' TODO this should go in _results
#'
#' @param stands TODO
#' @param unique_weights TODO
#' @param group_by TODO
#' @param output_fields TODO
#' @return
#'
#' @importFrom dplyr %>%
#'
compile_planning_areas_and_stands <- function(stands, unique_weights, group_by, output_fields) {
  group_planning_areas <- stands %>% dplyr::group_by_at(group_by)
  planning_areas <- data.table::data.table(group_planning_areas %>% dplyr::summarize(dplyr::across(output_fields, sum, .names = "ESum_{.col}" )))
  return (planning_areas)
}


#' Takes discrete minimum, maximum, and step values and turns them into
#' the string that forsys expects
#'
#' @param min Minimum value
#' @param max Maximum value
#' @param step Increment value
#' @return String of step values
#' @export
weight_values_to_string <- function(min = 0, max = 5, step = 1) {
  paste0(min, ' ', max, ' ', step)
}

#' Inverse of weight_values_to_string
#'
#' @param weight_str A string of minimum, maximum, and step weights
#' @return A list of values cast as integers
#' @export
weight_string_to_values <- function(weight_str) {
  vals <- stringr::str_split(weight_str, ' ')
  # message(vals)
  vals <- sapply(vals, as.numeric)
  # message(vals[,1])
}

#' Print the version information for Forsys.R
#'
#' @return A string representing the version of Forsys.R
#' @export
version <- function() {ver <- paste('forsys version: ', utils::packageVersion('forsys'))}

