#' Simulate landscape projects (wrapper)
#'
#' @param geom sf. Geometry representing stands
#' @param St_id vector. stands IDs. Converted to character
#' @param St_area numeric vector. Stands area
#' @param St_objective numeric vector. Stands objective values
#' @param St_seed numeric vector. Stands IDs seeds to search (n)
#' @param P_size numeric. Project size
#' @param P_size_min numeric. Minimum project size (when `P_constraint` is specified).
#' @param P_number integer. Number of patches to simulate
#' @param St_threshold character. Boolean statement on stand availability (e.g., 'field > 0.5')
#' @param SDW numeric 0-1. Objective weight parameter. Higher objectives are preferentially sought at higher SDW. Default is 0.5. 
#' @param EPW numeric 0-1. Exclusion weight parameter. Excluded stands are avoided at higher EPW. Default is 0.5. 
#' @param exclusion_limit numeric 0-1. Maximum percent of patch area that can be excluded.
#' @param P_constraint numeric vector. Stands values for the project constraint. If NULL, then  no secondary project constraint is not applied.
#' @param P_constraint_max_value numeric. Project constraint upper value. Default is Inf
#' @param P_constraint_min_value numeric. Project constraint lower value. Default is -Inf
#' @param sample_frac numeric 0-1. Portion of stands to search 
#' @param write_output logical. Write intermediate patch output to file
#'
#' @return list with first element describing patch-level stats and the second
#'   element describing stand-level stats.
#'
#' @details This function is meant as a API for calling patchmax within ForSys
#'   (or another environment) using a minimal set of parameters.
#'   
#' @export

build_patches <- function(
    geom,
    St_id,
    St_area,
    St_objective, 
    St_seed = NULL,
    P_size,
    P_size_min = -Inf,
    P_number = 1, 
    P_ceiling = NULL,
    P_ceiling_max = Inf,
    St_threshold = NULL, 
    SDW = .5,
    EPW = .5,
    exclusion_limit = 0.5,
    P_constraint = NULL, 
    P_constraint_max_value = Inf,
    P_constraint_min_value = -Inf,
    sample_frac = 0.1,
    sample_seed = NULL,
    adj_method = 'queen',
    adj_network = NULL,
    verbose = F,
    write_output = F
){
  
  if(!any(class(geom) == 'sf')){
    stop('Running forsys with Patchmax requires the stand data be saved as a sf object')
  }
  
  # append input data to stand geometry object
  geom$Id = St_id
  geom$Area = St_area
  geom$Objective = St_objective
  geom$Constraint = P_constraint
  
  # set P ceiling to 1 if NULL
  if(is.null(P_ceiling)){
    P_ceiling <- rep(1, length(St_id))
  }
  
  # create new patchmax instance
  pm <- patchmax::patchmax$new(
    geom = geom, 
    id_field = 'Id',
    objective_field = 'Objective', 
    area_field = 'Area', 
    area_max = P_size, 
    area_min = P_size_min,
    sdw = SDW,
    epw = EPW,
    threshold = St_threshold,
    exclusion_limit = exclusion_limit,
    seed = sample_seed,
    adj_method = adj_method,
    adj_network = adj_network)
  
  if(!is.null(P_constraint)){
    pm$params <- list(
      constraint_field = 'Constraint',
      constraint_max = P_constraint_max_value,
      constraint_min = P_constraint_min_value)
  }
  
  pm$random_sample(sample_frac = sample_frac)
  
  # simulate patches until count or ceiling exceeded
  repeat {
    
    # search and build best patch
    pm$search(show_progress = T, verbose = verbose)$build()
    
    # break loop if stop switch triggered
    if(pm$stop_switch == TRUE){
      message('No seeds to search')
      break
    }
    
    # increment area count
    csum <- ifelse(
      test = is.null(pm$patch_stats), 
      yes = 0, 
      no = sum(pm$patch_stats$area)) + 
      ifelse(
        test = is.null(pm$pending_patch$area), 
        yes = 0, 
        no = pm$pending_patch$area)
    
    # break loop if project ceiling is reached
    if (csum > P_ceiling_max) {
      message('Project ceiling reached')
      break
    } 
    
    # increment project count
    pcnt <- ifelse(is.null(pm$patch_stats), 0, nrow(pm$patch_stats)) + 1
    
    # break loop if project count is reached
    if (pcnt > P_number) {
      message('Project count reached')
      break
    } 
    
    # record patch info
    pm$record(write = write_output)
  }

  if(pm$patch_count > 0){
    # output patch statistics
    out_a <- pm$patch_stats %>% select(
      Project = patch_id,
      Area = area,
      TotalArea = coverage,
      Objective = objective,
      Constraint = constraint
    )
    
    # output stand data
    out_b <- pm$patch_stands %>% select(
      Project = patch_id,
      Stands = 2,
      DoTreat = include,
      Area = area,
      Objective = objective,
      Constraint = constraint
    )
  } else {
    out_a <- NULL
    out_b <- NULL
  }

  out <- list(out_a, out_b)
  return(out)
}
