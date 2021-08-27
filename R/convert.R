#------------------------------------------------
#' Convert amino acid abbreviation
#'
#' `convert_single()` converts the 1-letter abbreviation to the 3-letter
#' abbreviation and `convert_three()` does the opposite.
#'
#'
#' @param str String containing amino acids to convert.
#'
#' @aliases convert
#' @export
#' @examples
#' # Convert 1-letter abbreviation
#' convert_single("G235L")
#'
#' # Convert 3-letter abbreviation
#' convert_three("Gly235Leu")
convert_single <- function(str) {
  pattern <- stringr::str_c(aa_conversion$single, collapse = "|")
  fun <- function(query) {
    dplyr::filter(
      aa_conversion,
      stringr::str_to_lower(.data$single) == stringr::str_to_lower(query)
    )[2]
  }

  aa_change <- stringr::str_replace_all(str, pattern, fun)

  return(aa_change)
}

#' @rdname convert_single
#' @export
convert_three <- function(str) {
  lower <- stringr::str_to_lower(str)
  pattern <- stringr::str_c(stringr::str_to_lower(aa_conversion$three),
    collapse = "|"
  )
  fun <- function(query) {
    dplyr::filter(
      aa_conversion,
      stringr::str_to_lower(.data$three) == query
    )[1]
  }

  aa_change <- stringr::str_replace_all(lower, pattern, fun)

  return(aa_change)
}
