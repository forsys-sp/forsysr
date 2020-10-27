
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
selectSimpleGreedyAlgorithm <- function(dt = dbfFile, 
                                        grouped_by = 'PA_ID', 
                                        prioritize_by = 'TVMBF_SPM', 
                                        tally_by = 'AREA_HA', 
                                        grouped_target = 'AREA_PA10P'){
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
standFilter <- function(dt, filters){
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
createGroupedDataset <- function(dt, 
                                 grouping_vars, 
                                 summing_vars){
  ## Create the grouped data.table by grouping the treated subunits from the previous step.
  dt <- subset(dt[treat==1])
  dt <- group_by_at(dt, vars(grouping_vars))
  dt <- data.table(summarize_at(dt, .vars = vars(summing_vars), .funs = c(sum="sum")))
  names(dt) <- gsub(x = names(dt), pattern = "_sum", replacement = "")  
  return(dt)
}


#' Weight priorities for selection
#' 
#' @param minimum The minimum value of the weighting schema.
#' @param maximum The maximum value of the weighting schema.
#' @return A datatable with the weighted values for the priorities in the \code{priorityList}.
weightPriorities <- function(numPriorities, weights = c("0 10 1")){
  if(numPriorities == 1)
    return(data.table(1))
  weights <- strtoi(unlist(strsplit(weights, " ")))
  weights <- seq(weights[1], weights[2], weights[3])
  weightCombinations <- data.table(expand.grid(weights, weights))
  uniqueWeightCombinations <- data.table(createWeightedPairs(weightCombinations))
  zero_one <- data.table(Var1 = 0:1,
                         Var2 = 1:1)
  uniqueWeightCombinations <- rbind(uniqueWeightCombinations[,c("Var1", "Var2")], zero_one)
  
  return(uniqueWeightCombinations)
}

createWeightedPairs <- function(dt){
  dt <- dt[Var1 != Var2]
  dt[, proportion := ifelse(Var2 != 0, Var1 / Var2, 0)]
  dt <- dt[!duplicated(dt$proportion) ]
  return(dt)
}


#' Update target area for treatment after each activity
#' 
#' @param originalStands
#' @param treatedStands
#' @param subunit
#' @return A table with the updated subunit targets for all planning areas that had treatments
updateTarget <-function(treatedStands, subunit, unit_area){
  treatedSubunitTarget <- createGroupedDataset(treatedStands,
                                               subunit,
                                               unit_area)
  return(treatedSubunitTarget)
}

printSpecsDocument <- function(subunit, priorities, timber_threshold, volume_constraint){
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

addTargetField <- function(stands, pa_unit, pa_target, pa_target_multiplier, project_area, land_base){
  if(length(land_base) == 0){
    stands_updated <- standDT[, ':='(paste0(pa_target), (sum(get(pa_unit)))), by = stand_group_by]
  }else{
    stands_updated <- standDT[get(land_base) == 1, ':='(paste0(pa_target), (sum(get(pa_unit)))), by = stand_group_by]
  }
  return(stands_updated)
}

calculateSPMPCP <- function(stands, filter, fields){
  for(f in fields){
    if(length(filter) == 0) {
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
    else{
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
