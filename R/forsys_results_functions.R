safe_colorblind_palette <- c("#88CCEE", "#CC6677", "#DDCC77", "#117733", "#332288", "#AA4499",
                             "#44AA99", "#999933", "#882255", "#661100", "#6699CC", "#888888")

# read_results_file <- function(path) {
#   # TODO check for file type, but probably will be CSV
# 	readr::read_csv(path)
# }

#' TODO
#'
#' @param results_data TODO
#' @return TODO
#'
#' @importFrom dplyr %>%
#'
cumulate_results <- function(results_data) {
	results_data %>%
		dplyr::mutate()
}

#' Pass-through function while we shore up the graphing logic. This just sends ALL column
#' names back to the caller
#'
#' @param results_data TODO
#' @return List of priorities identified in results file
#'
#'
#' @importFrom dplyr %>%
#'
#' @export
get_results_columns <- function(results_data) {
	cols = results_data %>% colnames()
}

#' Right now this is a dumb function that returns all ETrt columns. Ideally we should have
#' logic that detects only the targets in the output file
#'
#' @param results_data TODO
#' @return List of priorities identified in results file
#'
#'
#' @importFrom dplyr %>%
#'
#' @export
get_result_targets <- function(results_data) {
	targets <- results_data %>%
				colnames() %>%
				purrr::keep(function(x) {stringr::str_detect(x, 'ETrt')})
}

#' Get a list of priorities used in the run. This takes a results file (loaded object)
#' and parses it, so a loaded scenario isn't necessary. It looks for the ETrt*PCP columns
#'
#' @param results_data TODO
#' @return List of priorities identified in results file
#'
#' @importFrom dplyr %>%
#'
#' @export
get_result_priorities <- function(results_data) {
	targets <- results_data %>%
				colnames() %>%
				purrr::keep(function(x) {stringr::str_detect(x, 'ETrt')}) # %>%
				purrr::keep(function(x) {stringr::str_detect(x, 'PCP')}) %>%
				purrr::map(function(x) {stringr::str_sub(x, 6)}) %>%
				purrr::map(function(x) {stringr::str_sub(x, 1, -5)})
}

#' From a priority (i.e. TVMBF) and get the associated Pr_ column
#'
#' @param results_data TODO
#' @param priority TODO
#' @return List of priorities identified in results file
#'
#' @importFrom dplyr %>%
#'
#'
#' @export
priority_column_name <- function(results_data, priority) {
	priority <- stringr::str_replace(priority, 'ETrt', '')
	priority <- results_data %>%
					colnames() %>%
					purrr::keep(function(x) {stringr::str_detect(x, 'Pr_')}) %>%
					purrr::keep(function(x) {stringr::str_detect(x, priority)})
}

#' TODO
#'
#' @param priority TODO
#' @return TODO
#'
#' @export
get_result_pcp_name <- function(priority) {
	paste0('ETrt_', priority, '_PCP')
}

#' TODO
#'
#' @param results_data The results data frame, probably starts with 'proj_'
#' @param pcp_fields PCPs to chart, will be the Y axis values. 
#' @param constraint_field Field used to constrain simulation. Usually some form of area.
#' @param priority The weight of priority to select. Different combinations of weights result in different outcomes
#' @return TODO
#'
#' @importFrom dplyr %>%
#'
#' @export
attainment_chart_by_target_treated <- function(results_data, pcp_fields, constraint_field, priority) {
	
	pcp_field <- pcp_fields[1]
	priority_name <- pcp_field

	priority_name <- stringr::str_replace(priority_name, 'ETrt', 'Pr_1')
	priority_name <- stringr::str_replace(priority_name, '_PCP', '')

	g <- results_data %>%
		dplyr::filter(.data[[priority_name]] == priority) %>%
		dplyr::mutate(x = cumsum(.data[[constraint_field]])) %>%
		dplyr::mutate(y = cumsum(.data[[pcp_field]]))

	print(g)

	g %>%
		ggplot2::ggplot() %>%
		+ ggplot2::geom_line(mapping=ggplot2::aes(x, y)) %>%
		+ ggplot2::labs(title='Attainment By Priority', x=constraint_field, y=pcp_field) %>%
		+ ggplot2::theme_set(ggplot2::theme_bw())

	if (length(pcp_fields) > 1) {
		for (p in 2:length(pcp_fields)) {
			pcp_field <- pcp_fields[p]
			q <- results_data %>%
				dplyr::filter(.data[[priority_name]] == priority) %>%
				dplyr::mutate(x = cumsum(.data[[constraint_field]])) %>%
				dplyr::mutate(y = cumsum(.data[[pcp_field]]))
			g %>% ggplot2::geom_line(mapping=ggplot2::aes(x, y))
		}
	}

}

#' TODO
#'
#' @param results_data TODO
#' @param proj_field TODO
#' @param x_field TODO
#' @param y_field TODO
#' @param constraint_field TODO
#' @return TODO
#'
#' @importFrom dplyr %>%
#'
#' @export
production_frontiers_chart <- function(results_data, proj_field, x_field, y_field, constraint_field) {
	# First, find the top PA_IDs in terms of target performance
	# Right now it's set to top 10, maybe make this dynamic?
	dat <- results_data %>%
			dplyr::group_by_at(proj_field) %>%
			dplyr::summarize(sum = sum(get(constraint_field))) %>%
			dplyr::slice_max(n = 10, order_by = sum)

	# print(dat)

	# Now, for those top 10, chart the x vs y of them
	top_proj_ids <- results_data[results_data[[proj_field]] %in% dat[[proj_field]], ]

	# TODO make x scale dynamic (scale_x_continuous)
	ggplot2::ggplot(top_proj_ids, ggplot2::aes(x = get(x_field), y = get(y_field), group = factor(proj_field), color = factor(proj_field))) +
	  ggplot2::geom_line() + 
	  ggplot2::theme_classic() + 
	  ggplot2::scale_color_manual(values = safe_colorblind_palette) +
	  directlabels::geom_dl(ggplot2::aes(label = factor(proj_field)), method = list(directlabels::dl.combine("first.points", "last.points")), cex = 0.8) + 
	  ggplot2::scale_x_continuous(expand=c(0, .1)) +
	  ggplot2::labs(title='Tradeoff Analysis', x = x_field, y = y_field, color = "PA_ID")
}

#' TODO
#'
#' @param field TODO
#' @param operator TODO
#' @param value TODO
#' @return TODO
#'
#' @importFrom dplyr %>%
#'
#' @export
parse_availability <- function(field, value) {
	parsed_string <- paste(field, operator, value)
}

#' TODO
#'
#' @param field TODO
#' @param operator TODO
#' @param value TODO
#' @return TODO
#'
#' @importFrom dplyr %>%
#'
#' @export
parse_thresholds <- function(field, operator, value) {
	parsed_string <- paste("treatment_name", field, operator, value)
}
