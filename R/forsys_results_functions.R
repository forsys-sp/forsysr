safe_colorblind_palette <- c("#88CCEE", "#CC6677", "#DDCC77",
                             "#117733", "#332288", "#AA4499",
                             "#44AA99", "#999933", "#882255",
                             "#661100", "#6699CC", "#888888")

main_title <- 22
p_titles <- 18
p_text <- 16


#' Pass-through function while we shore up the graphing logic. This just sends
#' ALL column names back to the caller
#'
#' @param results_data The results data frame
#'
#' @return List of priorities identified in results file
#'
#' @importFrom dplyr %>%
#'
#' @export
results_columns <- function(results_data) {
  cols <- results_data %>% colnames()
}

#' Toggle function for switching between light and dark modes
#'
#' @param trigger TODO
#'
#' @return TODO
#'
#' @export
theme_toggle <- function(trigger) {
  if (trigger == "Dark") {
    theme_palette <<- '#F2F2F2'
    grid_color <<- '#C0C0C0'
  } else {
    theme_palette <<- '#1A1A1A'
    grid_color <<- '#808080'
  }
  return (theme_palette)
}

#' For safety and sanity, take a string and rip off common output prefixes and
#' suffixes
#'
#' @param str_name The string to strip
#' @return The stripped string
#'
#' @importFrom dplyr %>%
#'
#' @export
strip_str <- function(str_name) {
  s <- str_name %>%
        stringr::str_replace("ETrt_", "") %>%
        stringr::str_replace("ESUM_", "") %>%
        stringr::str_replace("_PCP", "") %>%
        stringr::str_replace("_SPM", "")
}

# #' Right now this is a dumb function that returns all ETrt columns. Ideally
# #' we should have
# #' logic that detects only the targets in the output file
# #'
# #' @param results_data TODO
# #' @return List of priorities identified in results file
# #'
# #'
# #' @importFrom dplyr %>%
# #'
# #' @export
# get_result_targets <- function(results_data) {
#   targets <- results_data %>%
#         colnames() %>%
#         purrr::keep(function(x) {stringr::str_detect(x, 'ETrt')})
# }

#' Get a list of priorities used in the run. This takes a results file
#' (loaded object) and parses it, so a loaded scenario isn't necessary. It
#' looks for the ETrt*PCP columns
#'
#' @param column_name The column to parse out the priority name from
#' @return Parsed priority name which can be used to match a Pr_ column
#'
#' @importFrom dplyr %>%
#'
#' @export
parse_priority_name <- function(column_name) {
  priority <- column_name %>%
                stringr::str_replace("ETrt_", "") %>%
                stringr::str_replace("_PCP", "")
}

#' Get a list of priorities used in the run. This takes a results file
#' (loaded object) and parses it, so a loaded scenario isn't necessary. It
#' looks for the ETrt*PCP columns
#'
#' @param results_data The results data frame
#' @return List of priorities identified in results file
#'
#' @importFrom dplyr %>%
#'
#' @export
list_results_etrt_pcp_columns <- function(results_data) {
  targets <- results_data %>%
              colnames() %>%
              purrr::keep(function(x) {stringr::str_detect(x, "ETrt")}) %>%
              purrr::keep(function(x) {stringr::str_detect(x, "PCP")})
}

#' Get a list of priorities used in the run. This takes a results file
#' (loaded object) and parses it, so a loaded scenario isn't necessary. It
#' looks for the ETrt*PCP columns
#'
#' @param results_data The results data frame, probably starts with 'proj_'
#' @return List of priorities identified in results file
#'
#' @importFrom dplyr %>%
#'
#' @export
list_results_pr_columns <- function(results_data) {
  targets <- results_data %>%
              colnames() %>%
              purrr::keep(function(x) {stringr::str_detect(x, "Pr_")})
}

#' From a priority (i.e. TVMBF) and get the associated Pr_ column
#'
#' @param results_data The results data frame
#' @param priority TODO
#' @return List of priorities identified in results file
#'
#' @importFrom dplyr %>%
#'
#'
#' @export
priority_column_name <- function(results_data, priority) {
  priority <- results_data %>%
                colnames() %>%
                purrr::keep(function(x) {stringr::str_detect(x, "Pr_")}) %>%
                purrr::keep(function(x) {stringr::str_detect(x, priority)})
}

#' Take a priority string and append ETrt_*_PCP to it
#'
#' @param priority The name of a priority to turn into a ETrt_*_PCP string
#' @return The ETrt_priority_PCP string
#'
#' @export
priority_etrt_pcp_name <- function(priority) {
  # Just in case, strip the string to be sure it's just the name
  p <- strip_str(priority)
  paste0("ETrt_", p, "_PCP")
}

#' TODO
#'
#' @param results_data The results data frame
#' @param priority The NAME of the main priority. This will be the filter for
#' weight, and will add ETrt_*_PCP to access the value field
#' @param constraint_field Field used to constrain simulation. Usually some
#' form of area.
#' @param secondary_effects (optional) Any other NAMES to select. It will
#' automatically add ETrt_*_PCP to the names
#' @return TODO
#'
#' @importFrom rlang .data
#' @importFrom dplyr %>%
#' @importFrom dplyr across
#'
#' @export
attainment_data_filter <- function(results_data, priority, constraint_field, secondary_effects=c()) {
  # Get the ETrt_*_PCP field name based on the priority
  priority_pcp_field <- priority_etrt_pcp_name(priority)
  # If there are secondary effects, also get their ETrt_*_PCP names
  secondary_fields <- lapply(secondary_effects, priority_etrt_pcp_name) %>% unlist()
  # Lastly get the ETrt_*_PCP name for the constraint. Note it might come in ok already, but this guarantees it
  # constraint_pcp_field = priority_etrt_pcp_name(constraint_field)
  constraint_pcp_field <- paste0("ETrt_", constraint_field)

  # Get the "Pr" field corresponding to the selected priority
  priority_weight_field <- priority_column_name(results_data, priority)

  # List all "Pr" fields in the results table
  weight_columns <- list_results_pr_columns(results_data)

  # Check the length of weight_columns. A single priority run will have only one, otherwise we need to negate
  # the main priority and select only the run where the other priorities had 0 weight
  if (length(weight_columns) > 1) {
    other_priority <- weight_columns[stringr::str_which(weight_columns, priority_weight_field, negate=TRUE)]
    results_data <- results_data %>% dplyr::filter(.data[[other_priority]] == 0)
  }

  # Sort on treatment rank JUST IN CASE it got out of order somewhere, then append the x and y columns
  g <- results_data %>%
        dplyr::arrange(.data[["treatment_rank"]]) %>%
        dplyr::mutate(x = cumsum(.data[[constraint_pcp_field]])) %>%
        dplyr::mutate("y_{priority_pcp_field}" := cumsum(.data[[priority_pcp_field]])) %>%
        dplyr::mutate(across(secondary_fields, cumsum, .names = "y_{.col}"))

}

#' TODO
#'
#' @param attainment_data The prepared data from from attainment_data_filter
#' @return TODO
#'
#' @importFrom rlang .data
#' @importFrom dplyr %>%
#'
attainment_data_format <- function(attainment_data) {
  foi <- c("treatment_rank", "x", list_attainment_targets(attainment_data))
  data <- attainment_data[, foi]
  data_long <- data %>%
                tidyr::gather(pcp, value, -c(treatment_rank, x)) %>%
                dplyr::mutate(pcp = stringr::str_replace(pcp, "y_ETrt_", "")) %>%
                dplyr::mutate(pcp = stringr::str_replace(pcp, "_PCP", ""))
}

#' Get a list of 'y' columns, calculated in attainment_data_filter, which show
#' cumulative attainment in terms of PCP
#'
#' @param results_data The results data frame
#' @return List of attainment columns
#'
#' @importFrom dplyr %>%
#'
list_attainment_targets <- function(results_data) {
  targets <- results_data %>%
              colnames() %>%
              purrr::keep(function(x) {stringr::str_detect(x, "y_")})
}

#' TODO
#'
#' @param results_data The results data frame
#' @param priority The NAME of the main priority. This will be the filter for
#' weight, and will add ETrt_*_PCP to access the value field
#' @param constraint_field Field used to constrain simulation. Usually some
#' form of area.
#' @param secondary_effects (optional) Any other NAMES to select. It will
#' automatically add ETrt_*_PCP to the names
#' @return TODO
#'
#' @importFrom rlang .data
#' @importFrom dplyr %>%
#' @importFrom ggplot2 aes
#' @importFrom ggplot2 geom_line
#' @importFrom ggplot2 labs
#' @importFrom ggplot2 element_text
#' @importFrom ggplot2 element_rect
#' @importFrom ggplot2 element_line
#' @importFrom ggplot2 rel
#'
#' @export
attainment_chart_by_target_treated <- function(results_data, priority, constraint_field, secondary_effects=c()) {
  g <- attainment_data_filter(results_data, priority, constraint_field, secondary_effects)
  g_long <- attainment_data_format(g)

  p <- g_long %>%
      ggplot2::ggplot() %>%
      + ggplot2::aes(x = x, y = value, color = pcp) %>%
      + ggplot2::geom_line(size = 2) %>%
      + ggplot2::labs(title="Attainment By Priority", x = "Cumulative Area Treated", y = "Cumulative Outcome", colour = "") %>%
      + ggplot2::theme_set(ggplot2::theme_minimal()) %>%
      + ggplot2::scale_color_discrete() %>%
      + ggplot2::scale_x_continuous(labels = function(x) {format(x, scientific=FALSE)}) %>%
      + ggplot2::theme(title = element_text(color = theme_palette)) %>%
      + ggplot2::theme(legend.background = element_rect(fill = 'transparent', color = NA)) %>%
      + ggplot2::theme(plot.background = element_rect(fill = 'transparent', color = NA)) %>%
      + ggplot2::theme(panel.background = element_rect(fill = 'transparent', color = NA)) %>%
      + ggplot2::theme(panel.grid.major = element_line(color = grid_color, 0.5)) %>%
      + ggplot2::theme(panel.grid.minor = element_line(color = grid_color, 0.5)) %>%
      + ggplot2::theme(axis.text.x = element_text(size = 11)) %>%
      + ggplot2::theme(axis.text.y = element_text(size = 11)) %>%
      + ggplot2::theme(axis.title.x = element_text(size = 13)) %>%
      + ggplot2::theme(axis.title.y = element_text(size = 13)) %>%
      + ggplot2::theme(legend.title = element_text(size = 13)) %>%
      + ggplot2::theme(legend.text = element_text(size = 11, color = theme_palette)) %>%
      + ggplot2::theme(legend.key = element_rect(fill = NA, size = 11,  color = NA)) %>%
      + ggplot2::theme(plot.title = element_text(size = 17))

  return(p)
}

#' TODO
#'
#' @param results_data The results data frame
#' @param priority The NAME of the main priority. This will be the filter for weight, and will
#' add ETrt_*_PCP to access the value field
#' @param constraint_field Field used to constrain simulation. Usually some form of area.
#' @param secondary_effects (optional) Any other NAMES to select. It will automatically add
#' ETrt_*_PCP to the names
#' @return TODO
#'
#' @importFrom rlang .data
#' @importFrom dplyr %>%
#' @importFrom ggplot2 element_text
#' @importFrom ggplot2 element_rect
#' @importFrom ggplot2 element_line
#' @importFrom ggplot2 rel
#'
#' @export
cumulative_attainment_chart <- function(results_data, priority, constraint_field, secondary_effects=c()) {
  g <- attainment_data_filter(results_data, priority, constraint_field, secondary_effects)
  g_long <- attainment_data_format(g)

  p <- g_long %>%
      ggplot2::ggplot() %>%
      + ggplot2::aes(x = x, y = value, fill = pcp) %>%
      + ggplot2::geom_area(size = 2, alpha = 0.7) %>%
      + ggplot2::labs(title="Cumulative Attainment By Priority", x = "Cumulative Area Treated", y = "Cumulative Outcome", fill = "") %>%
      + ggplot2::theme_set(ggplot2::theme_minimal()) %>%
      + ggplot2::scale_color_discrete() %>%
      + ggplot2::scale_x_continuous(labels = function(x) {format(x, scientific=FALSE)}) %>%
      + ggplot2::theme(legend.background = element_rect(fill = 'transparent', color = NA)) %>%
      + ggplot2::theme(plot.background = element_rect(fill = 'transparent', color = NA)) %>%
      + ggplot2::theme(panel.background = element_rect(fill = 'transparent', color = NA)) %>%
      + ggplot2::theme(title = element_text(color = theme_palette)) %>%
      + ggplot2::theme(axis.text.x = element_text(size = p_text)) %>%
      + ggplot2::theme(axis.text.y = element_text(size = p_text)) %>%
      + ggplot2::theme(axis.title.x = element_text(size = p_titles)) %>%
      + ggplot2::theme(axis.title.y = element_text(size = p_titles)) %>%
      + ggplot2::theme(legend.title = element_text(size = p_titles)) %>%
      + ggplot2::theme(legend.text = element_text(size = p_text, color = theme_palette)) %>%
      + ggplot2::theme(panel.grid.major = element_line(color = grid_color)) %>%
      + ggplot2::theme(panel.grid.minor = element_line(color = grid_color)) %>%
      + ggplot2::theme(plot.title = element_text(size = main_title))

  return(p)
}


#' TODO
#'
#' @param results_data TODO
#' @param proj_field The planning area or project ID
#' @param x_field The first priority to look at. Should be the priority name, e.g. "TVMIN", 
#' which will be parsed to "ETrt_TVMIN_PCP"
#' @param y_field The second priotity to look at
#' @return TODO
#'
#' @importFrom dplyr %>%
#'
#' @export
tradeoff_analysis_chart <- function(results_data, proj_field, x_field, y_field) {

  # Save the names for labels
  x_name <- x_field
  y_name <- y_field

  x_field <- priority_etrt_pcp_name(x_field)
  y_field <- priority_etrt_pcp_name(y_field)

  results_data['rank'] <- results_data[x_field] + results_data[y_field]

  # First, find the top PA_IDs in terms of target performance
  # Right now it's set to top 10, maybe make this dynamic?
  dat <- results_data %>%
      dplyr::group_by_at(proj_field) %>%
      dplyr::summarize(sum = sum(get('rank'))) %>%
      dplyr::slice_max(n = 10, order_by = sum)

  # print(dat)

  # Now, for those top 10, chart the x vs y of them
  top_proj_ids <- results_data[results_data[[proj_field]] %in% dat[[proj_field]], ]

  # TODO make x scale dynamic (scale_x_continuous)
  p <- top_proj_ids %>%
        ggplot2::ggplot() %>%
        + ggplot2::aes(x = get(x_field), y = get(y_field), group = factor(get(proj_field)), color = factor(get(proj_field))) %>%
        + ggplot2::geom_line() %>%
        + directlabels::geom_dl(ggplot2::aes(label = factor(get(proj_field))), method = list(directlabels::dl.combine("first.points", "last.points")), cex = 0.8) %>%
        # ggplot2::scale_x_continuous(expand=c(0, .1)) +
        + ggplot2::labs(title="Tradeoff Analysis", x = x_name, y = y_name, color = proj_field) %>%
        + ggplot2::theme_set(ggplot2::theme_minimal()) %>%
        + ggplot2::scale_color_discrete() %>%
        + ggplot2::theme(legend.background = element_rect(fill = 'transparent', color = NA)) %>%
        + ggplot2::theme(plot.background = element_rect(fill = 'transparent', color = NA)) %>%
        + ggplot2::theme(panel.background = element_rect(fill = 'transparent', color = NA)) %>%
        + ggplot2::theme(title = element_text(color = theme_palette)) %>%
        + ggplot2::theme(axis.text.x = element_text(size = p_text, color = theme_palette)) %>%
        + ggplot2::theme(axis.text.y = element_text(size = p_text,color = theme_palette)) %>%
        + ggplot2::theme(axis.title.x = element_text(size = p_titles, color = theme_palette)) %>%
        + ggplot2::theme(axis.title.y = element_text(size = p_titles, color = theme_palette)) %>%
        + ggplot2::theme(legend.title = element_text(size = p_titles)) %>%
        + ggplot2::theme(legend.text = element_text(size = p_text, color = theme_palette)) %>%
        + ggplot2::theme(panel.grid.major = element_line(color = grid_color)) %>%
        + ggplot2::theme(panel.grid.minor = element_line(color = grid_color)) %>%
        + ggplot2::theme(plot.title = element_text(size = main_title))

  return(p)
}

#' TODO
#'
#' @param subset_data The subset data results data frame
#' @param priority The NAME of the main priority. This will be the filter for
#' weight, and will add ETrt_*_PCP to access the value field
#' @param proj_field TODO
#' @param constraint_field Field used to constrain simulation. Usually some
#' form of area.
#' @param group_field TODO
#' @return TODO
#'
#' @importFrom dplyr %>%
#' @importFrom rlang .data
#'
#' @export
stacked_barchart <- function(subset_data, priority, proj_field, constraint_field, group_field) {
  priority_pcp_field <- priority_etrt_pcp_name(priority)
  priority_weight_field <- priority_column_name(subset_data, priority)

  # List all "Pr" fields in the results table
  weight_columns <- list_results_pr_columns(subset_data)

  # Check the length of weight_columns. A single priority run will have only one, otherwise we need to negate
  # the main priority and select only the run where the other priorities had 0 weight
  if (length(weight_columns) > 1) {
    other_priority <- weight_columns[stringr::str_which(weight_columns, priority_weight_field, negate=TRUE)]
    subset_data <- subset_data %>% dplyr::filter(.data[[other_priority]] == 0)
  }

  g <- subset_data %>%
        dplyr::select(c(proj_field, group_field, priority_pcp_field, 'treatment_rank')) %>%
        dplyr::filter(.data[["treatment_rank"]] <= 10) %>%
        dplyr::group_by_at(c(proj_field, group_field)) %>%
        dplyr::summarize(s = sum(.data[[priority_pcp_field]]))

  p <- g %>%
        ggplot2::ggplot() %>%
        + ggplot2::aes(x = factor(get(proj_field)), y = s, fill = factor(get(group_field))) %>%
        + ggplot2::geom_bar(position = "stack", stat = "identity") %>%
        + ggplot2::labs(title=paste("Attainment By", group_field), subtitle = "Top Projects", x = proj_field, y = "Cumulative Outcome", fill = group_field) %>%
        + ggplot2::theme_set(ggplot2::theme_minimal()) %>%
        + ggplot2::scale_color_discrete(name = group_field) %>%
        + ggplot2::theme(legend.background = element_rect(fill = 'transparent', color = NA)) %>%
        + ggplot2::theme(plot.background = element_rect(fill = 'transparent', color = NA)) %>%
        + ggplot2::theme(panel.background = element_rect(fill = 'transparent', color = NA)) %>%
        + ggplot2::theme(axis.text.x = element_text(size = p_text, color = theme_palette)) %>%
        + ggplot2::theme(axis.text.y = element_text(size = p_text,color = theme_palette)) %>%
        + ggplot2::theme(axis.title.x = element_text(size = p_titles, color = theme_palette)) %>%
        + ggplot2::theme(axis.title.y = element_text(size = p_titles, color = theme_palette)) %>%
        + ggplot2::theme(legend.title = element_text(size = p_titles, color = theme_palette)) %>%
        + ggplot2::theme(legend.text = element_text(size = p_text, color = theme_palette)) %>%
        + ggplot2::theme(panel.grid.major = element_line(color = grid_color)) %>%
        + ggplot2::theme(panel.grid.minor = element_line(color = grid_color)) %>%
        + ggplot2::theme(plot.title = element_text(size = main_title, color = theme_palette))
  
  return(p)
}

#' Custom scale for the boxplot, this implements an inverted logrithmic scale
#' 
#' @param base The logrithmic base to use
#' @return 
reverselog_trans <- function(base = exp(1)) {
    trans <- function(x) -log(x, base)
    inv <- function(x) base^(-x)
    scales::trans_new(paste0("reverselog-", format(base)),
                      trans,
                      inv,
                      log_breaks(base = base),
                      domain = c(1e-100, Inf))
}

#' TODO
#'
#' @param results_data The results data frame
#' @param proj_field The planning area or project ID
#' @param x_field The NAME of the first priority. This will be the filter for
#' weight, and will add ETrt_*_PCP to access the value field
#' @param y_field The NAME of the secondary priority. This will be used to 
#' rank projects between weighting scenarios
#' @param constraint_field Field used to constrain simulation. Usually some
#' form of area.
#'
#' @return TODO
#'
#' @importFrom dplyr %>%
#'
#' @export
project_boxplot <- function(results_data, proj_field, x_field, y_field, constraint_field) {
  # First, find the top PA_IDs in terms of target performance
  # Right now it's set to top 10, maybe make this dynamic?

  x_field <- priority_etrt_pcp_name(x_field)
  y_field <- priority_etrt_pcp_name(y_field)
  constraint_field <- priority_etrt_pcp_name(constraint_field)

  results_data['rank'] <- results_data[x_field] + results_data[y_field]

  # First, find the top PA_IDs in terms of target performance
  # Right now it's set to top 10, maybe make this dynamic?
  dat <- results_data %>%
      dplyr::filter(.data[["treatment_rank"]] > 0) %>% # Check that treatment_rank is not NA first
      dplyr::group_by_at(proj_field) %>%
      dplyr::summarize(sum = sum(get('rank'))) %>%
      dplyr::slice_max(n = 10, order_by = sum)


  # Now, for those top 10, chart the x vs y of them
  top_proj_ids <- results_data[results_data[[proj_field]] %in% dat[[proj_field]], ]

  p <- top_proj_ids %>%
        ggplot2::ggplot() %>%
        + ggplot2::aes(x = get("proj_field"), y = get("treatment_rank"), color = factor(get(proj_field))) %>%
        + ggplot2::geom_boxplot() %>%
        + ggplot2::scale_y_continuous(trans=reverselog_trans(10), expand = c(0, .2)) %>%
        + ggplot2::labs(title="Treatment Rank Distribution", x = proj_field, y = "Treatment Rank", names = get("proj_field"), caption = "Y axis is scaled using an inverse log10 transform.", horizontal = TRUE, color = proj_field) %>%
        + directlabels::geom_dl(ggplot2::aes(label = factor(get(proj_field))), method = list("bottom.points", cex = 1.5, vjust = 1.5), position = position_dodge(width = .75)) %>%
        + ggplot2::theme_set(ggplot2::theme_minimal()) %>%
        + ggplot2::scale_color_discrete() %>%
        + ggplot2::theme(legend.background = element_rect(fill = 'transparent', color = NA)) %>%
        + ggplot2::theme(plot.background = element_rect(fill = 'transparent', color = NA)) %>%
        + ggplot2::theme(panel.background = element_rect(fill = 'transparent', color = NA)) %>%
        + ggplot2::theme(axis.text.x = element_blank()) %>%
        + ggplot2::theme(axis.text.y = element_text(size = p_text,color = theme_palette)) %>%
        + ggplot2::theme(axis.title.x = element_text(size = p_titles, color = theme_palette)) %>%
        + ggplot2::theme(axis.title.y = element_text(size = p_titles, color = theme_palette)) %>%
        + ggplot2::theme(legend.title = element_text(size = p_titles, color = theme_palette)) %>%
        + ggplot2::theme(legend.text = element_text(size = p_text, color = theme_palette)) %>%
        + ggplot2::theme(plot.caption = element_text(size = p_text, color = theme_palette, hjust = 0, face = "italic", lineheight = 4)) %>%
        + ggplot2::theme(panel.grid.major.x = element_line(color = NA)) %>%
        + ggplot2::theme(panel.grid.major = element_line(color = grid_color)) %>%
        + ggplot2::theme(panel.grid.minor = element_line(color = grid_color)) %>%
        + ggplot2::theme(plot.title = element_text(size = main_title, color = theme_palette))

  return(p)
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
  return(parsed_string)
}
