
load_dataset <- function(path_to_file) {
  file_type <- str_sub(path_to_file, start= -3)
  print("Loading Dataset")
  print(file_type)
  if (file_type == "dbf") {
    standDT <- data.table(read.dbf(path_to_file))
  } else if (file_type == "csv") {
    standDT <- data.table(fread(path_to_file, header = TRUE))
  } else {
    ('Input format not recognized')
  }
  return (standDT)
}


#' Select subunits to treat based on a given priority.
#'
#' @param dt A data table with all the subunits and attributes.
#' @param grouped_by The management objective that is being prioritized.
#' @param prioritize_by The stand value for the priority that is used to constrain the results.
#' Typically Area or Volume.
#' @param tally_by The variable that is summed to meet the \code{tally_target}.
#' @param group_target The variable that contains the subunit values which are summed to meet the constraint value.
#' @param filter_by The variable that contains the values that are filtered by the thresholdVal.
#' i.e. this value must be greater than the threshold value.
#' @param filter_threshold The
#' @return The selected stands from \code{df}, ordered by \code{prioritize_by}, and selected until the sum of \code{tally_by} is as close to
#' \code{group_target} as possible.
select_simple_greedy_algorithm <- function(dt = dbfFile,
                                        grouped_by = 'PA_ID',
                                        prioritize_by = 'TVMBF_SPM',
                                        tally_by = 'AREA_HA',
                                        grouped_target = 'AREA_PA10P') {
  # For each grouped_by:
  # Remove subunits that don't meet threshold:
  # Order by priority:
  # Select stands up to treatment target
  # The operator in the line below determines the directionality and whether the threshold is inclusive or not.
  dt <- data.table(dt)
  dt[, considerForTreatment := 1]
  dt[, current_target := 0]
  dt[, cumulative_tally_by := 0]
  dt[, treat := 0]
  # This recursive function selects subunits until either targets have been met or all available subunits have been checked.
  #dt <- recursiveSelection(dt, grouped_by, prioritize_by, tally_by, grouped_target)
  # Common issue: if the dataset has na values in the priority field, this will fail.
  while(sum(dt[,considerForTreatment==1 & treat == 0]) != 0){
    # Order the data table by the priority.
    dt <- dt[order(dt[,get(grouped_by)], -dt[,get(prioritize_by)])]
    # Determine the current target
    dt[, current_target := 0]
    dt[treat == 1, current_target := sum(get(tally_by)), by=list(get(grouped_by))]
    dt[, current_target := get(grouped_target) - max(current_target), by=list(get(grouped_by))]
    # Compute the cumulative sum for the constrained variable and select subunits
    dt[considerForTreatment == 1 & treat == 0, cumulative_tally_by := cumsum(get(tally_by)), by=list(get(grouped_by))]
    dt[considerForTreatment == 1 & treat == 0 & cumulative_tally_by <= current_target, treat := 1 ]
    # Determine the remaining target
    dt[, current_target := 0]
    dt[treat == 1, current_target := sum(get(tally_by)), by=list(get(grouped_by))]
    dt[, current_target := get(grouped_target) - max(current_target), by=list(get(grouped_by))]
    # Remove from treatment consideration if subunit value is greater than total target.
    dt[considerForTreatment == 1 & treat == 0, considerForTreatment := ifelse(get(tally_by) > current_target, 0, 1 )]
  }
  dt <- dt[treat == 1,]
  return(dt)
}

#' Produce a set of stands that can be treated under a given criteria.
#' @param dt A data table with all stand information necessary to determine availability for a specific treatment type.
#' @param filters A list of strings that are used to filter the stands for treatment availability.
#' @return The final data table with stands available for treatment.
stand_filter <- function(dt, filters) {
  for(f in 1:nrow(filters)){
    filter <- paste0(filters[f,2], " ", filters[f,3], " ", filters[f,4])
    filter <- str_remove(filter, "'")
    filter <- str_remove(filter, "'")
    dt <- subset(dt, eval(parse(text = filter)))
  }
  ## Hard-coded manageable/undisturbed.
  #dt <- subset(dt, man_alldis == 1)
  return(dt)
}


#' Create a dataset by subsetting subunits that were selected in the selectSubunits function
#' and grouping the data by a larger subunit (usually planning areas).
#'
#' @param dt A data table with all the subunits and attributes.
#' @param grouping_vars The variable names by which the data will be grouped.
#' @param summing_vars The variables in the original dataset that need to be summed over each subunit.
#' @return The selected stands from \code{df}, ordered by \code{priority_SPM}, and selected until the sum of \code{priority_STND} is as close to
#' \code{treat_target} as possible.
create_grouped_dataset <- function(dt,
                                 grouping_vars,
                                 summing_vars) {
  ## Create the grouped data.table by grouping the treated subunits from the previous step.
  dt <- subset(dt[treat==1])
  dt <- group_by_at(dt, vars(grouping_vars))
  dt <- data.table(summarize_at(dt, .vars = vars(summing_vars), .funs = c(sum="sum")))
  names(dt) <- gsub(x = names(dt), pattern = "_sum", replacement = "")
  return(dt)
}

weight_values_to_string <- function(min = 0, max = 5, step =1) {
  glue(min, ' ', max, ' ', step)
}

weight_string_to_values <- function(weight_str) {
  vals <- str_split(weight_str, ' ')
  # print(vals)
  vals <- sapply(vals, as.numeric)
  # print(vals[,1])
}

#' Weight priorities for selection
#'
#' @param minimum The minimum value of the weighting schema.
#' @param maximum The maximum value of the weighting schema.
#' @return A datatable with the weighted values for the priorities in the \code{priorityList}.
weight_priorities <- function(numPriorities, weights = c("1 1 1")){
  if(numPriorities == 1)
    return(data.table(1))
  weights <- strtoi(unlist(strsplit(weights, " ")))
  weights <- seq(weights[1], weights[2], weights[3])
  # Updates by Luke Wilkerson to incorporate multiple priorities.
  weightPermute <- (permutations(length(weights), numPriorities, weights, repeats.allowed=TRUE))
  weightprops <- proportions(weightPermute, 1)
  weightPermute <- data.table(weightPermute)
  uniqueWeightCombinations <- weightPermute[!duplicated(weightprops) & rowSums(weightPermute) != 0, ]

  return(uniqueWeightCombinations)
}

createWeightedPairs <- function(dt) {
  dt <- dt[Var1 != Var2]
  dt[, proportion := ifelse(Var2 != 0, Var1 / Var2, 0)]
  dt <- dt[!duplicated(dt$proportion) ]
}


#' Update target area for treatment after each activity
#'
#' @param treated_stands TODO
#' @param subunit TODO
#' @param unit_area TODO
#' @return A table with the updated subunit targets for all planning areas that had treatments
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
                       "1) ForSys output - pa_all.csv")
  writeLines(parameters, file("README.txt"))

}

# TODO Add function description
add_target_field <- function(stands, pa_unit, pa_target, pa_target_multiplier, stand_group_by, land_base) {
  if(length(land_base) == 0) {
    stands_updated <- stands[, ':='(paste0(pa_target), (sum(get(pa_unit)))), by = stand_group_by]
  } else {
    stands_updated <- stands[get(land_base) == 1, ':='(paste0(pa_target), (sum(get(pa_unit)))), by = stand_group_by]
  }
  return(stands_updated)
}

# TODO Add function description
calculate_spm_pcp <- function(stands, filter, fields){
  for (f in fields) {
    if (length(filter) == 0) {
      maximum <- max(stands[, get(f)])
      cn <- paste0(f, "_SPM")
      expr <- bquote(.(as.name(cn)):= 0)
      stands[,eval(expr)]
      expr <- bquote(.(as.name(cn)):= (100 * get(f) / maximum))
      stands[, eval(expr)]

      sum.total <- sum(as.numeric(stands[, get(f)]))
      cn <- paste0(f, "_PCP")
      expr <- bquote(.(as.name(cn)):= 0)
      stands[,eval(expr)]
      expr <- bquote(.(as.name(cn)):= (100 * get(f) / sum.total))
      stands[, eval(expr)]
    }
    else {
      maximum <- max(stands[get(filter) == 1, get(f)])
      cn <- paste0(f, "_SPM")
      expr <- bquote(.(as.name(cn)):= 0)
      stands[,eval(expr)]
      expr <- bquote(.(as.name(cn)):= (100 * get(f) / maximum))
      stands[get(filter) == 1, eval(expr)]

      sum.total <- sum(as.numeric(stands[get(filter) == 1, get(f)]))
      cn <- paste0(f, "_PCP")
      expr <- bquote(.(as.name(cn)):= 0)
      stands[,eval(expr)]
      expr <- bquote(.(as.name(cn)):= (100 * get(f) / sum.total))
      stands[get(filter) == 1, eval(expr)]
    }
  }
  return(stands)
}

# This just gets names that calculate_spm_pcp would create, for use in the UI
get_spm_pcp_names <- function(fields) {
  l <- vector('list', length(fields) * 2)
  i <- 1
  for (f in fields) {
    l[i] <- paste0(f, '_SPM')
    l[i+1] <- paste0(f, '_PCP')
    i <- i + 2
  }
  return(unlist(l))
  # nms <- sapply(fields, function(f) {c(paste0(f, "_SPM"), paste0(f, "_PCP"))})
}

# Inverse function of get_spm_pcp_names, it returns the opposite of the selected
# fields for suggested priority names
get_priority_output_names <- function(fields) {
  l <- vector('list', length(fields))
  i <- 1
  for (f in fields) {
    g <- str_replace(f, 'STND_', '')

    if (str_detect(g, 'SPM')) {
      l[i] <- str_replace(g, 'SPM', 'PCP')
      
    } else {
      l[i] <- str_replace(g, 'PCP', 'SPM')
    }

    i <- i + 1
  }

  return(unlist(l))
}

# Hack the area target - can be set in the shapefile.
# This code may be updated for looping multiple treatment types.
# It should do the same thing that Pedro is working on within a single run instead of wrapped.
set_up_treatment_types <- function(stands, args=NULL) {
  stands$Commercial <- stands$Commercial*stands$AREA_HA
  stands$PreCommercial <- stands$AREA_HA - stands$Commercial
  stands$treat <- 0
  stands$treatment_type <- ""
  stands$treatedPAArea <- 0
  return(stands)
}

set_up_priorities_helper <- function(i, stands, weight, priority) {
  stands$weightedPriority <- stands$weightedPriority + weight * stands[, get(priority)]
  priorityName <- paste0("Pr_", i, "_", priority)
  stands[, (priorityName) := weight]
}

set_up_priorities <- function(w, priorities, weights, stands) {

  for (i in 1:ncol(weights)) { # START FOR 1
    curr_weight = weights[[i]][w]
    curr_priority = priorities[[i]][1]

    stands <- set_up_priorities_helper(i, stands, curr_weight, curr_priority)
  } # END FOR 1


  return(stands)
}

make_thresholds <- function(thresholds) {
  all_thresholds <- NULL
  all_thresholds <- sapply(1:length(thresholds), function(i) {
    all_thresholds <- rbind(all_thresholds, strsplit(thresholds[i], " ")[1])
  })
}

apply_treatment <- function(treatment_types,
                            stands,
                            all_thresholds,
                            stand_group_by, 
                            stand_field, 
                            fixed_target, fixed_area_target=NULL,
                            pa_unit=NULL, pa_target=NULL, pa_target_multiplier=NULL) {
  stands_updated <- stands
  selected_stands <- NULL

  for (t in 1:length(treatment_types)) {
    filtered_stands <- stand_filter(stands, all_thresholds[V1 == treatment_types[t], ])
    print(paste0("There are ", nrow(filtered_stands), " stands that meet treatment thresholds for ", treatment_types[t]))
    # Set the target for each bin here:
    print(paste0("Treatment type: ", all_thresholds[t,1]))

    # TODO insert checks for proper fixed vs pa variables
    if (fixed_target == TRUE) {
      filtered_stands <- set_fixed_area_target(filtered_stands, fixed_area_target) # Target based on fixed field
    } else if (fixed_target == FALSE) {
      filtered_stands <- set_percentage_area_target(filtered_stands, pa_target, pa_target_multiplier) # Activate for percentage not fixed area
    }

    # The following code splits out the planning area treatments by treatment type and percent of area treated. An area (rather than %)
    # area target can be set by removing the multiplier and replacing AREA_MAN with the area target in hectares (i.e. 2000).
    # if(all_thresholds[t,1] == c("Commercial")){
    #   filtered_stands[, current_area_target := AREA_MAN *.15 - treatedPAArea]
    # }else if(all_thresholds[t,1] == c("HF")){
    #   filtered_stands[, current_area_target := AREA_MAN * .30 - treatedPAArea]
    #   filtered_stands[, weightedPriority := WHPMN_SPM]
    # }else{
    #   print("Missing Threshold Update! Treatment selection is suspect!")
    # }
    treat_stands <- select_simple_greedy_algorithm(dt = filtered_stands,
                                          grouped_by = stand_group_by,
                                          prioritize_by = "weightedPriority",
                                          tally_by = pa_unit,
                                          grouped_target = "current_area_target")

    # This updates the total area available for activities. Original treatment target - total area treated for each subunit (planning area).

    area_treatedPA <- update_target(treat_stands, stand_group_by, pa_unit)
    stands_updated <- stands_updated[area_treatedPA,  treatedPAArea := treatedPAArea + i.sum, on = stand_group_by]
    stands_updated <- stands_updated[treat_stands, ':='(treatment_type = treatment_types[t], treat = 1), on = stand_field]
    selected_stands <- rbind(selected_stands, stands_updated[treat==1,])
  }

  return(selected_stands)
}

set_fixed_area_target <- function(stands, fixed_area_target) {
  stands[, current_area_target := fixed_area_target]
}

set_percentage_area_target <- function(stands, pa_target, pa_target_multiplier) {
  stands[, current_area_target := get(pa_target) * pa_target_multiplier]
}

# Step 3: Identify the best planning areas within each nest.
identify_nested_planning_areas <- function(grouped_by_pa) {

  # Step 3: Identify the best planning areas within each nest.
  print("Selecting planning area subunits")
  groupedByPA$system <- 1

  if (system_constraint == TRUE) {
    grouping_type <- c("system")
    groupedByPA[,harvestTarg := sum(groupedByPA[,get(nesting_target) * get(nesting_unit)])]

  } else {
    grouping_type <- nesting_unit
    groupedByPA[,harvestTarg := groupedByPA[,get(nesting_target) * get(nesting_unit)]]
  }

  groupedByPA$treat <- 0
  paSubunits <- select_simple_greedy_algorithm(dt = groupedByPA,
                                            grouped_by = grouping_type,
                                            prioritize_by = "weightedPriority",
                                            tally_by = nesting_unit,
                                            grouped_target = "harvestTarg")
  paSubunits <- paSubunits[order(-paSubunits$weightedPriority),]
  print("adding treatment rank")
  paSubunits$treatment_rank <- seq(1:nrow(paSubunits))
}

write_stand_outputs_to_file <- function(dir, unique_weights, name, selected_stands) {
    stand_output_file <- paste0(dir, "/",  name, "_stands", unique_weights, ".csv")
    fwrite(selected_stands, stand_output_file)
}

compile_planning_areas_and_stands <- function(unique_weights, stands, group_by, output_fields) {
  group_planning_areas <- stands %>% group_by_at(group_by)

  planning_areas <- data.table(summarize_at(group_planning_areas, .vars = vars(output_fields), .funs = c(sum="sum")))
  names(planning_areas) <- gsub(x = names(planning_areas), pattern = "_sum", replacement = "")
  sum_names <- paste0("ESum_", colnames(planning_areas[,output_fields, with = FALSE]), sep = "")
  setnames(planning_areas, c(output_fields), c(sum_names))

  return (planning_areas)
}
