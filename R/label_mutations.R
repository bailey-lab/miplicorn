#------------------------------------------------
#' Label mutations
#'
#' @description
#' For each data point, label a mutation as `"ref"`, `"alt"`, `"ins"`, or
#' `"del"`.
#'
#' * Data points that have more reference (REF) than alternate (ALT) calls will
#'   be labeled as `"ref"`.
#' * Mutations whose REF and ALT calls are both one base pair and who have more
#'   ALT calls than REF calls will be labeled as `"alt"`.
#' * Mutations whose REF and ALT calls differ in length and have more ALT calls
#'   than REF calls will be labeled as `"ins"`.
#' * Mutations whose REF and ALT calls differ in length and have more REF calls
#'   than ALT calls will be labeled as `"del"`.
#'
#' @param .data The data set containing REF and ALT calls.
#' @param .before,.after `r lifecycle::badge("experimental")`
#'   <[`tidy-select`][dplyr_tidy_select]> Optionally, control where new columns
#'   should appear (the default is to add to the right hand side). See
#'   [`dplyr::relocate()`][dplyr::relocate()] for more details.
#'
#' @export
#' @examples
#' # Read example data
#' data <- read_tbl_ref_alt_cov(
#'   miplicorn_example("reference_AA_table.csv"),
#'   miplicorn_example("alternate_AA_table.csv"),
#'   miplicorn_example("coverage_AA_table.csv"),
#'   gene == "atp6"
#' )
#'
#' # Add ref and alt calls to data
#' sequences <- c("A", "T", "C", "G", "AT", "TC", "TGC")
#' data <- dplyr::mutate(
#'   data,
#'   ref = sample(sequences, size = nrow(data), replace = TRUE),
#'   alt = sample(sequences, size = nrow(data), replace = TRUE)
#' )
#'
#' # Label the mutations
#' label_mutations(data)
#' label_mutations(data, .after = alt)
label_mutations <- function(.data, .before = NULL, .after = NULL) {
  UseMethod("label_mutations")
}

#' @export
label_mutations.default <- function(.data, .before = NULL, .after = NULL) {
  cli_abort(c(
    "Cannot label the mutations of this data object.",
    "i" = "Object must be a reference, alternate, coverage table.",
    "i" = "Object must additionally contain reference and alternate calls."
  ))
}

#' @importFrom stringr str_length
#' @rdname label_mutations
#' @export
label_mutations.ref_alt_cov_tbl <- function(.data,
                                            .before = NULL,
                                            .after = NULL) {
  .before <- enquo(.before)
  .after <- enquo(.after)

  # Ensure only one of .before and .after exist
  if (!rlang::quo_is_null(.before) && !rlang::quo_is_null(.after)) {
    cli_abort("Must supply only one of `.before` and `.after`.")
  }

  # Label the mutations
  dplyr::mutate(.data,
    ans_der_indel = dplyr::case_when(
      ref_umi_count > alt_umi_count ~ "ref",
      str_length(ref) == 1 & str_length(alt) == 1 & alt_umi_count > ref_umi_count ~ "alt",
      str_length(ref) < str_length(alt) & alt_umi_count > ref_umi_count ~ "ins",
      str_length(ref) > str_length(alt) & alt_umi_count > ref_umi_count ~ "del",
      TRUE ~ NA_character_
    ),
    .before = !!.before,
    .after = !!.after
  )
}

# To silence the R CMD Check
globalVariables(c("ref", "alt"))
