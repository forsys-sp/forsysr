
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
#' @export
#' 
filter_stands <- function(stands, filter_txt, verbose = TRUE){
  tryCatch({
    out <- NULL # helps devtools::check()
    out <- subset(stands, eval(parse(text = filter_txt)))
    n0 <- nrow(stands)
    n1 <- nrow(out)
    if(verbose)
      message(glue::glue("----------\nFiltering stands where: {filter_txt} ({round((n0-n1)/n0*100,2)}% excluded)\n-----------"))
  }, error = function(e){
    message(paste0('!! Filter failed; proceeding with unfiltered data. Error message:\n', print(e)))
  })
  return(out)
}

#' Calculate spm for specified fields
#'
#' Used to normalize stands attributes by dividing each field by its maximum
#' value and multiplying by 100. Values are first divided by area if the
#' `area_field` is provides.
#'
#' @param stands stand data
#' @param fields vector of character field names to calculate spm values
#' @param area_field optional string of field name used to calculate spm
#'
#' @importFrom dplyr pull mutate
#' @export
#' 
calculate_spm <- function(stands, fields=NULL, area_field=NULL, availability_txt=NULL){

  # filter for availability
  include = TRUE
  if(!is.null(availability_txt)){
    eval_txt <- paste0("stands %>% mutate(out = ifelse(",filter_txt,", TRUE, FALSE)) %>% pull(out)")
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
#' 
#' @importFrom dplyr pull mutate
#' @export
#'
calculate_pcp <- function(stands, fields=NULL, availability_txt=NULL){
  
  # filter for availability
  include = TRUE
  if(!is.null(availability_txt)){
    eval_txt <- paste0("stands %>% mutate(out = ifelse(",filter_txt,", TRUE, FALSE)) %>% pull(out)")
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