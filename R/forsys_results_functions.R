
# read_results_file <- function(path) {
#   # TODO check for file type, but probably will be CSV
# 	readr::read_csv(path)
# }

cumulate_results <- function(results_data) {
	results_data %>%
		mutate()
}

#' Right now this is a dumb function that returns all ETrt columns. Ideally we should have
#' logic that detects only the targets in the output file
#'
#' @param results_data TODO
#' @return List of priorities identified in results file
get_result_targets <- function(results_data) {
	targets <- results_data %>%
				colnames() %>% 
				keep(function(x) {str_detect(x, 'ETrt')})
}

#' Get a list of priorities used in the run. This takes a results file (loaded object) 
#' and parses it, so a loaded scenario isn't necessary. It looks for the ETrt*PCP columns
#'
#' @param results_data TODO
#' @return List of priorities identified in results file
get_result_priorities <- function(results_data) {
	targets <- results_data %>%
				colnames() %>% 
				keep(function(x) {str_detect(x, 'ETrt')}) %>%
				keep(function(x) {str_detect(x, 'PCP')}) %>%
				map(function(x) {str_sub(x, 6)}) %>%
				map(function(x) {str_sub(x, 1, -5)})
}

#' From a priority (i.e. TVMBF) and get the associated Pr_ column
#'
#' @param results_data TODO
#' @param priority TODO
#' @return List of priorities identified in results file
priority_column_name <- function(results_data, priority) {
	priority <- results_data %>%
					colnames() %>%
					keep(function(x) {str_detect(x, 'Pr_')}) %>%
					keep(function(x) {str_detect(x, priority)})
}

get_result_pcp_name <- function(priority) {
	paste0('ETrt_', priority, '_PCP')
}

attainment_chart_by_target_treated <- function(results_data, pcp_field, target_field, priority) {
	g <- results_data %>%  
		filter(get(priority) == 1.0) %>%
		mutate(x = cumsum(get(target_field))) %>% 
		mutate(y = cumsum(get(pcp_field))) 
		
	g %>% 
		ggplot() %>%
		+ geom_line(mapping=aes(x, y)) %>%
		+ labs(title='Attainment By Priority', x=target_field, y=pcp_field) %>%
		+ theme_set(theme_bw())

}

production_frontiers_chart <- function(results_data, x_field, y_field, target_field) {
	# First, find the top PA_IDs in terms of target performance
	# Right now it's set to top 10, maybe make this dynamic?
	dat <- results_data %>%
			group_by(PA_ID) %>% 
			summarize(sum = sum(get(target_field))) %>%
			slice_max(n = 10, order_by = sum)


	print(dat)

	# Now, for those top 10, chart the x vs y of them
	top_pa_ids <- results_data[results_data$PA_ID %in% dat$PA_ID, ]

	# g <- top_pa_ids %>%
	# 	ggplot() %>%
	# 	+ aes(x=x_field, y=y_field) %>%
	# 	+ geom_point() %>%
	# 	+ labs(title='Production Frontiers', x=x_field, y=y_field) %>%
	# 	+ theme_set(theme_bw())	

	g <- ggplot(data=top_pa_ids, aes(x=x_field, y=y_field)) + geom_line()
}
