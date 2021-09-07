#------------------------------------------------
#' Sort rows by column values
#'
#' `sort()` orders the rows of a data frame by the values of selected columns.
#'
#' @details
#' ## Ordering
#' Data is sorted in ascending order.
#'
#' ## Natural sorting
#' `sort()` is built on `dplyr::arrange()` to provide
#' \href{https://en.wikipedia.org/wiki/Natural_sort_order}{natural sorting}
#' (sorting of strings with both letters and numerals). The underlying
#' implementation for natural sorting is based on the `stringi` library.
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
#' * Columns are not modified.
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
#' sort(df, sample, gene)
#' df %>% sort(sample, gene)
sort <- function(.data, ...) {
  # Store variables to sort by
  sort_vars <- enquos(...)

  if (requireNamespace("stringi", quietly = TRUE) &
    requireNamespace("purrr", quietly = TRUE)) {
    # Manipulate variables
    sort_vars <- purrr::map(sort_vars, function(var) {
      expr(stringi::stri_rank(!!var, numeric = TRUE))
    })
  } else {
    warning("Packages \"purrr\" and \"stringi\" needed for natural sorting. Please install them.")
  }

  # Call dplyr::arrange
  .data %>%
    dplyr::arrange(!!!sort_vars)
}
