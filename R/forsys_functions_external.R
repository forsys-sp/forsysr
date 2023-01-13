#' Combine priorities
#'
#' Combines 2 or more priorities into a new field.
#'
#' @param stands Data frame containing stand data
#' @param fields Field names (2 or more) to combine
#' @param weights Numeric vector with weights. Assume equal weighting if NULL
#' @param new_field Name to assign combined priority
#' 
#' @import glue sf
#' @export
#' 
combine_priorities <- function(
    stands, 
    fields = NULL, 
    weights = NULL, 
    new_field = 'combined_priority',
    record_weights = FALSE
    ){
  
  if(is.null(weights) | length(weights) == 1){
    weights = rep(1, length(fields))
  }
  
  if(is.null(fields) | length(fields) != length(weights)){
    stop('Function requires >= 2 fields and a vector with weight values of equal length ')
  }
  
  sp <- stands %>% st_drop_geometry() %>% select(fields)
  cp <- apply(sp * weights, 1, sum)
  stands <- stands %>% mutate(!!new_field := cp)
  message(glue('Combined priority assigned to: ', new_field))
  
  if(record_weights){
    for(i in 1:length(fields)){
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
filter_stands <- function(stands, filter_txt = NULL, verbose = TRUE){
  if(is.null(filter_txt)){
    return(stands)
  }
  tryCatch({
    out <- NULL # helps devtools::check()
    out <- subset(stands, eval(parse(text = filter_txt)))
    n0 <- nrow(stands)
    n1 <- nrow(out)
    if(verbose)
      message(glue("----------\nFiltering stands where: {filter_txt} ({round((n0-n1)/n0*100,2)}% excluded)\n-----------"))
  }, error = function(e){
    message(paste0('!! Filter failed; proceeding with unfiltered data. Error message:\n', print(e)))
  })
  return(stands)
}

#' Calculate spm for specified fields
#'
#' Used to normalize stands attributes by dividing each field by its maximum
#' value and multiplying by 100. Values are first divided by area if the
#' `area_field` is provides.
#'
#' @param stands stand data
#' @param fields vector of character field names to calculate spm values
#' @param availability_txt Boolean statement describing stand availability
#'
#' @details Unavailable stands are given a value of zero.
#' 
#' @importFrom dplyr pull mutate
#' @export
#' 
calculate_spm <- function(stands, fields=NULL, area_field=NULL, availability_txt=NULL){

  # filter for availability
  include = TRUE
  if(!is.null(availability_txt)){
    eval_txt <- paste0(
      "stands %>% mutate(out = ifelse(", 
      availability_txt,
      ", TRUE, FALSE)) %>% pull(out)")
    include = eval(parse(text = eval_txt))
  }
  
  # default to calculating spm for all numeric fields if fields is null 
  if(is.null(fields)){
    x <- stands %>% lapply(is.numeric) %>% unlist()
    fields <- names(x)[x == TRUE]
  }
  
  for (f in fields) {
      values <- pull(stands, f)
      values[include == FALSE] <- 0
      maximum <- max(values, na.rm=T)
      spm_values <- (100 * values / maximum)
      cn <- paste0(f, "_SPM")
      stands <- stands %>% mutate(!!cn := spm_values)
  }
  
  return(stands)
}

#' Calculate pcp for specified fields
#'
#' @param stands stand data
#' @param fields vector of character field names to calculate spm values
#' @param availability_txt Boolean statement describing stand availability
#' 
#' @details Unavailable stands are given a value of zero.
#' 
#' @importFrom dplyr pull mutate
#' @export
#'
calculate_pcp <- function(stands, fields=NULL, availability_txt=NULL){
  
  # filter for availability
  include = TRUE
  if(!is.null(availability_txt)){
    eval_txt <- paste0("stands %>% mutate(out = ifelse(", availability_txt,", TRUE, FALSE)) %>% pull(out)")
    include = eval(parse(text = eval_txt))
  }
  
  # default to calculating spm for all numeric fields if fields is null 
  if(is.null(fields)){
    x <- stands %>% lapply(is.numeric) %>% unlist()
    fields <- names(x)[x == TRUE]
  }
  
  for (f in fields) {
    # calculate percent of total and multiple by 100
    cn <- paste0(f, "_PCP")
    values <- as.numeric(pull(stands, f))
    values[include == FALSE] <- 0
    sum.total <- sum(values, na.rm=T)
    stands <- stands %>% mutate(!!cn := (100 * values / sum.total))
  }
  
  return(stands)
}