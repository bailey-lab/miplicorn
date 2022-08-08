#------------------------------------------------
#' Plot coverage
#'
#' Create a bar chart visualizing the average coverage across a given grouping
#' variable. The color of each bar represents the magnitude of the mean
#' coverage.
#'
#' @param data A data frame, data frame extension (e.g. a tibble), or a lazy
#'   data frame (e.g. from dbplyr or dtplyr).
#' @param group_by The grouping variable. The average coverage is computed for
#'   each unique value of the variable.
#'
#' @return A [ggplot2][ggplot2::ggplot2-package] object.
#'
#' @export
#' @examples
#' # Read example data
#' data <- read_tbl_coverage(miplicorn_example("coverage_AA_table.csv"))
#'
#' # Plot coverage grouped by gene
#' plot_coverage(data, gene)
#' @export
plot_coverage <- function(data, group_by) {
  UseMethod("plot_coverage")
}

#' @export
plot_coverage.default <- function(data, group_by) {
  cli_abort(c(
    "Cannot plot the coverage with this data object.",
    "i" = "Object must be a coverage table or a reference, alternate, coverage table."
  ))
}

# Plotting function
plot_coverage_fn <- function(data, group_by) {
  # Ensure inputs exist
  rlang::check_required(data)
  rlang::check_required(group_by)

  # Group and create stats
  plot_data <- data %>%
    dplyr::group_by({{ group_by }}) %>%
    dplyr::summarise(mean_coverage = mean(.data$coverage, na.rm = TRUE))

  # Plot
  ggplot2::ggplot(
    data = plot_data,
    mapping = ggplot2::aes(
      x = {{ group_by }},
      y = .data$mean_coverage,
      fill = .data$mean_coverage
    )
  ) +
    ggplot2::geom_col() +
    default_theme() +
    ggplot2::theme(
      axis.text.x = ggplot2::element_text(angle = 90, vjust = 0.5, hjust = 1),
      legend.position = "none"
    )
}

#' @rdname plot_coverage
#' @export
plot_coverage.cov_tbl <- plot_coverage_fn

#' @rdname plot_coverage
#' @export
plot_coverage.ref_alt_cov_tbl <- plot_coverage_fn
