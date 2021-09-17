#------------------------------------------------
#' Arrange rows by column values naturally
#'
#' @description
#' `arrange_natural()` orders the rows of a data frame by the values of selected
#' columns.
#'
#' Data is naturally sorted (see *Details*, below) in ascending order. Columns
#' used for sorting are converted to factors to preserve their ordering.
#' Grouping is ignored.
#'
#' @details
#' ## Natural sorting
#' `arrange_natural()` is built on [dplyr::arrange()] to provide
#' \href{https://en.wikipedia.org/wiki/Natural_sort_order}{natural sorting}
#' (sorting of strings with both letters and numerals). The underlying
#' implementation for natural sorting is based on the
#' \href{https://stringi.gagolewski.com/}{`stringi`} library.
#'
#' @param .data A data frame, data frame extension (e.g. a tibble), or a lazy
#'   data frame (e.g. from dbplyr or dtplyr).
#' @param ... <[`data-masking`][dplyr_data_masking]> Variables to sort by.
#'
#' @return
#' An object of the same type as `.data`. The output has the following
#' properties:
#'
#' * All rows appear in the output, but (usually) in a different place.
#' * Sorted columns are converted to factors.
#' * All other columns are not modified.
#' * Data frame attributes are preserved.
#'
#' @seealso [dplyr::arrange()]
#' @export
#' @examples
#' df <- tibble::tribble(
#'   ~sample, ~gene,
#'   "D10-23", "atp6",
#'   "D10-43", "mdr1",
#'   "D10-55", "atp6",
#'   "D10-5", "mdr1",
#'   "D10-47", "dhps",
#'   "D10-15", "atp6"
#' )
#'
#' arrange_natural(df, sample)
#' df %>% arrange_natural(sample, gene)
arrange_natural <- function(.data, ...) {
  # Store variables to arrange by
  dots <- enquos(..., .named = TRUE)

  if (requireNamespace("stringi", quietly = TRUE) &
    requireNamespace("purrr", quietly = TRUE)) {
    # Manipulate dots to get arrange variables
    arrange_vars <- purrr::map(dots, function(var) {
      expr(stringi::stri_rank(as.character(!!var), numeric = TRUE))
    })

    # Manipulate dots to get mutate variables
    mutate_vars <- purrr::map(dots, function(var) {
      expr(forcats::as_factor(as.character(!!var)))
    })
  } else {
    rlang::warn('Packages "purrr" and "stringi" needed for natural sorting. Please install them.')
    return(dplyr::arrange(.data, !!!dots))
  }

  # Call dplyr::arrange
  .data %>%
    dplyr::arrange(!!!arrange_vars) %>%
    dplyr::mutate(!!!mutate_vars)
}
