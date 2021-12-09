#------------------------------------------------
#' Compute prevalence of mutations
#'
#' @param data A data frame, data frame extension (e.g. a tibble), or a lazy
#'   data frame (e.g. from dbplyr or dtplyr).
#' @param threshold A minimum UMI count which reflects the confidence in the
#'   genotype call. Data with a UMI count of less than the threshold will be
#'   filtered out from the analysis.
#'
#' @return
#' A `tibble` with the extra class `mutation_prev`. The output has the following
#' columns:
#'
#' * `mutation_name`: The unique mutation sequenced.
#' * `n_total`: The number of samples for which a mutation site was sequenced.
#' * `n_mutant`: The number of samples for which a mutation occurred.
#' * `prevalence`: The prevalence of the mutation.
#'
#' @export
mutation_prevalence <- function(data, threshold) {
  # Ensure have a table with reference umi counts, alternate umi counts, and
  # coverage
  cols <- c("ref_umi_count", "alt_umi_count", "coverage")
  if (!all(cols %in% colnames(data))) {
    abort(c(
      "Data is mising required columns.",
      x = "Need a column for the reference UMI counts.",
      x = "Need a column for the alternate UMI counts.",
      x = "Need a column for the coverage."
    ))
  }

  # Use threshold to filter data
  total <- dplyr::filter(
    data,
    coverage > threshold & (alt_umi_count > threshold | ref_umi_count > threshold)
  )

  mutant_data <- dplyr::filter(total, alt_umi_count > threshold)

  # Need column mutation name
  if (!"mutation_name" %in% colnames(data)) {
    abort("Data needs the column `mutation_name`.")
  }

  # Get counts of mutations
  total_count <- total %>%
    dplyr::count(mutation_name) %>%
    dplyr::rename(n_total = n)

  mutant_count <- mutant_data %>%
    dplyr::count(mutation_name) %>%
    dplyr::rename(n_mutant = n)

  # Compute prevalence
  prevalence <- total_count %>%
    dplyr::full_join(mutant_count, by = "mutation_name") %>%
    dplyr::mutate(prevalence = n_mutant / n_total)

  # Assign a subclass "mutation_prev"
  class(prevalence) <- c("mutation_prev", class(prevalence))

  prevalence
}
