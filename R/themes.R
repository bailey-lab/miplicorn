#------------------------------------------------
#' Custom themes
#'
#' These are custom `ggplot2` themes which control all non-data display. You
#' may further tweak components of these themes using [ggplot2::theme()].
#'
#' @param base_size Base font size, given in pts.
#' @param base_family Base font family.
#' @param base_line_size Base size for line elements.
#' @param base_rect_size Base size for rect elements.
#'
#' @importFrom ggplot2 %+replace%
#' @examples
#' library("ggplot2")
#' p <- ggplot(mtcars, aes(x = wt, y = mpg, colour = factor(gear))) +
#'   geom_point() +
#'   facet_wrap(~am) +
#'   geom_smooth(method = "lm", se = FALSE)
#'
#' p + theme_miplicorn()
#' p + theme_rainbow()
#' @name custom_themes
#' @aliases NULL
NULL

#' @rdname custom_themes
#' @export
theme_miplicorn <- function(base_size = 10,
                            base_family = "",
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

#' @rdname custom_themes
#' @export
theme_rainbow <- function(base_size = 12,
                          base_family = "",
                          base_line_size = base_size / 22,
                          base_rect_size = base_size / 22) {
  ggplot2::theme_bw(
    base_size = base_size,
    base_family = base_family,
    base_line_size = base_line_size,
    base_rect_size = base_rect_size
  ) %+replace%
    ggplot2::theme(
      plot.title = ggplot2::element_text(hjust = 0.5),
      axis.line.x = ggplot2::element_line(color = "black", size = 0.3),
      axis.line.y = ggplot2::element_line(color = "black", size = 0.3),
      panel.grid.major = ggplot2::element_blank(),
      panel.grid.minor = ggplot2::element_blank(),
      legend.position = "bottom",
      complete = TRUE
    )
}

# Need to have a function that returns the default theme which will be used in
# figures. This is according to the best practices for using ggplot2 in a
# package. Link:
# <https://ggplot2.tidyverse.org/articles/ggplot2-in-packages.html>
default_theme <- function() {
  theme_miplicorn()
}
rainbow_theme <- function() {
  theme_rainbow()
}
