#------------------------------------------------
#' Convert amino acid abbreviations
#'
#' `convert_single()` converts the 1-letter amino acid abbreviation to the
#' 3-letter amino acid abbreviation and `convert_three()` does the opposite.
#'
#' Conversion is case-insensitive, but always returns capitalized 1-letter and
#' 3-letter abbreviations in title-case.
#'
#' @param str Character vector containing amino acids to convert.
#'
#' @return Character vector with converted amino acid abbreviations.
#'
#' @aliases convert
#' @export
#' @examples
#' # Convert 1-letter abbreviation
#' convert_single("G235L")
#' convert_single("s545p")
#'
#' # Convert 3-letter abbreviation
#' convert_three("Gly235Leu")
#' convert_three("SER545LEU")
convert_single <- function(str) {
  # Generate regex pattern
  pattern <- stringr::str_c(aa_conversion$single, collapse = "|")

  # Function for replacing with 3-letter abbreviation
  convert <- function(query) {
    query <- stringr::str_to_lower(query)
    abbrv <- stringr::str_to_lower(aa_conversion$single)
    aa_conversion[query == abbrv, ]$three
  }

  # Replace pattern with function, ignoring case
  stringr::str_replace_all(
    string = str,
    pattern = stringr::regex(pattern, ignore_case = TRUE),
    replacement = convert
  )
}

#' @rdname convert_single
#' @export
convert_three <- function(str) {
  # Generate regex pattern
  pattern <- stringr::str_c(aa_conversion$three, collapse = "|")

  # Function for replacing with 1-letter abbreviation
  convert <- function(query) {
    query <- stringr::str_to_lower(query)
    abbrv <- stringr::str_to_lower(aa_conversion$three)
    aa_conversion[query == abbrv, ]$single
  }

  # Replace pattern with function, ignoring case
  stringr::str_replace_all(
    string = str,
    pattern = stringr::regex(pattern, ignore_case = TRUE),
    replacement = convert
  )
}
