empty_file <- function(x) {
  is.character(x) &&
    file.exists(x) &&
    file.info(x, extra_cols = FALSE)$size == 0
}

#------------------------------------------------
#' Custom ggplot2 theme
#'
#' Custom ggplot2 theme that builds off of `ggplot2::theme_classic()`.
#'
#' @inheritParams ggplot2::theme_classic
#'
#' @importFrom ggplot2 %+replace%
#' @export
#' @examples
#' library("ggplot2")
#' p <- ggplot(mtcars, aes(x = wt, y = mpg, colour = factor(gear))) +
#'   geom_point() +
#'   facet_wrap(~am) +
#'   geom_smooth(method = "lm", se = FALSE) +
#'   theme_miplicorn()
#' p
theme_miplicorn <- function(base_size = 10, base_family = "",
                            base_line_size = base_size / 22,
                            base_rect_size = base_size / 22) {
  ggplot2::theme_classic(
    base_size = base_size,
    base_family = base_family,
    base_line_size = base_line_size,
    base_rect_size = base_rect_size
  ) %+replace%
    ggplot2::theme(
      plot.title = ggplot2::element_text(hjust = 0.5, size = 12),
      legend.position = "right",
      complete = TRUE
    )
}

# Need to have a function that returns the default theme which will be used in
# figures. This is according to the best practices for using ggplot2 in a
# package. Link:
# <https://ggplot2.tidyverse.org/articles/ggplot2-in-packages.html#creating-a-new-theme-1>
default_theme <- function() {
  theme_miplicorn()
}
