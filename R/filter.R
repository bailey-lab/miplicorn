#' Filter function factory
#'
#' Define a function factory to create filtering functions that filter the
#' data based on a single column.
#'
#' @param .col The name of the column used to filter the data.
#' @param .type The type of the column.
#'
#' @seealso [Advanced R](<https://adv-r.hadley.nz/function-factories.html>) for
#'   more information on function factories.
#' @keywords internal
#' @examples
#' \dontrun{
#'
#' filter_coverage <- filter_fn_factory("coverage", "numeric")
#' filter_gene <- filter_fn_factory("gene", "character")
#' }
filter_fn_factory <- function(.col, .type = c("numeric", "character")) {
  force(.col)
  .type <- match.arg(.type)

  function(.data, .value, .preserve = FALSE) {
    # Check column existence
    if (!.col %in% colnames(.data)) {
      abort(glue("Data needs the column `{.col}`."))
    }

    # Urge users to use dplyr::filter() for more complex cases
    warn(
      message = c(
        "This function provides a simple filtering interface.",
        i = "For more complex filtering, please use `dplyr::filter()`."
      ),
      .frequency = "regularly",
      .frequency_id = .col,
    )

    switch(.type,
      numeric = dplyr::filter(
        .data,
        .data[[.col]] >= .value,
        .preserve = .preserve
      ),
      character = dplyr::filter(
        .data,
        .data[[.col]] == .value,
        .preserve = .preserve
      )
    )
  }
}

#------------------------------------------------
#' Subset rows using a single column value
#'
#' These `filter_*()` functions are used to subset a data frame. Each function
#' subsets rows based on the value of one column. For numeric columns, a row
#' will be kept if the value is greater than or equal to the value specified.
#' For categorical columns, a row will be kept if the value is equal to the
#' value specified. Note that when a condition evaluates to NA the row will be
#' dropped, unlike base subsetting with `[`.
#'
#' @param .data A data frame, data frame extension (e.g. a tibble), or a lazy
#'   data frame (e.g. from dbplyr or dtplyr).
#' @param .value The filtering value to be applied.
#' @inheritParams dplyr::filter
#'
#' @name filter
#' @seealso [dplyr::filter()] for more complex filtering operations.
#' @examples
#' data <- tibble::tribble(
#'   ~sample, ~gene, ~coverage,
#'   "S1", "atp6", 10,
#'   "S2", "crt", 20,
#' )
#' filter_gene(data, "atp6")
#' filter_coverage(data, 15)
NULL

#' @rdname filter
#' @export
filter_aa_change <- filter_fn_factory("aa_change", "character")

#' @rdname filter
#' @export
filter_alt_umi_count <- filter_fn_factory("alt_umi_count", "numeric")

#' @rdname filter
#' @export
filter_coverage <- filter_fn_factory("coverage", "numeric")

#' @rdname filter
#' @export
filter_gene <- filter_fn_factory("gene", "character")

#' @rdname filter
#' @export
filter_mutation_name <- filter_fn_factory("mutation_name", "character")

#' @rdname filter
#' @export
filter_ref_umi_count <- filter_fn_factory("ref_umi_count", "numeric")

#' @rdname filter
#' @export
filter_targeted <- filter_fn_factory("targeted", "character")
