
# read_results_file <- function(path) {
#   # TODO check for file type, but probably will be CSV
# 	readr::read_csv(path)
# }

cumulate_results <- function(results_data) {
	results_data %>%
		mutate()
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

production_frontiers_chart <- function(results_data) {
	
}
