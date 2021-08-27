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
  # Generate regex pattern
  pattern <- stringr::str_c(aa_conversion$single, collapse = "|")

  # Function for replacing with 3-letter abbreviation
  fun <- function(query) {
    dplyr::filter(
      aa_conversion,
      stringr::str_to_lower(.data$single) == stringr::str_to_lower(query)
    )[2]
  }

  # Replace pattern with function, ignoring case
  aa_change <- stringr::str_replace_all(
    str,
    stringr::regex(pattern, ignore_case = TRUE),
    fun
    )

  return(aa_change)
}

#' @rdname convert_single
#' @export
convert_three <- function(str) {
  # Generate regex pattern
  pattern <- stringr::str_c(aa_conversion$three, collapse = "|")

  # Function for replacing with 1-letter abbreviation
  fun <- function(query) {
    dplyr::filter(
      aa_conversion,
      stringr::str_to_lower(.data$three) == stringr::str_to_lower(query)
    )[1]
  }

  # Replace pattern with function, ignoring case
  aa_change <- stringr::str_replace_all(
    str,
    stringr::regex(pattern, ignore_case = TRUE),
    fun
  )

  return(aa_change)
}
