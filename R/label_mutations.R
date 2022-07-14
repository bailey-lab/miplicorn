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
#' data <- tibble::tribble(
#'   ~sample, ~pos, ~ref, ~alt, ~ref_umi_count, ~alt_umi_count, ~coverage,
#'   "S1", "1049838", "A", "G", 54, 10, 64,
#'   "S2", "801498", "G", "A", 15, 0, 15,
#'   "S3", "625403", "T", "C", 0, 15, 15,
#'   "S4", "748165", "GA", "G", 2, 18, 20,
#'   "S5", "487199", "G", "ATC", 0, 10, 10
#' )
#'
#' label_mutations(data)
#' label_mutations(data, .after = alt)
#' label_mutations(data, .before = pos)
label_mutations <- function(.data, .before = NULL, .after = NULL) {
  .before <- enquo(.before)
  .after <- enquo(.after)

  # Ensure only one .before and .after exist
  if (!rlang::quo_is_null(.before) && !rlang::quo_is_null(.after)) {
    cli_abort("Must supply only one of `.before` and `.after`.")
  }

  # Label the mutations
  dplyr::mutate(.data,
    ans_der_indel = dplyr::case_when(
      ref_umi_count > alt_umi_count ~ "ref",
      stringr::str_length(ref) == 1 & stringr::str_length(alt) == 1 & alt_umi_count > ref_umi_count ~ "alt",
      stringr::str_length(ref) < stringr::str_length(alt) & alt_umi_count > ref_umi_count ~ "ins",
      stringr::str_length(ref) > stringr::str_length(alt) & alt_umi_count > ref_umi_count ~ "del",
      TRUE ~ NA_character_
    ),
    .before = !!.before,
    .after = !!.after
  )
}
