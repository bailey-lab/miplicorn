#------------------------------------------------
#' Create annotated chromosome map
#'
#' @description
#' `r lifecycle::badge("deprecated")`
#'
#' This function has been deprecated as it was a simple wrapper around
#' `plot_chromoMap()` and `plot_karyoploteR()` and added unnecessary complexity
#' to function calls.
#'
#' @details
#' Render a graphics visualization of entire chromosomes or chromosomal regions.
#' Annotate multiple targeted regions to visualize probe targets.
#'
#' @param genome A tibble indicating the starting and ending position of each
#'   chromosome. Contains three columns:
#'   \itemize{
#'     \item Name of the chromosome
#'     \item The starting position of the chromosome
#'     \item The ending position of the chromosome.
#'   }
#' @param probes A tibble indicating the starting and ending position of each
#'   probe. Contains four columns:
#'   \itemize{
#'     \item Name of the chromosome the probe is on
#'     \item The starting position of the probe
#'     \item The ending position of the probe
#'     \item An identifier indicating the probe set the probe belongs to.
#'   }
#' @param map_pkg The package used for the underlying implementation of the
#'   chromosome map.
#' @param title The title of the plot.
#' @param colours A vector of colours indicating the annotation colour for each
#'   probe set.
#' @param ... <[`dynamic-dots`][rlang::dyn-dots]> Additional arguments passed to
#'   internal plotting functions.
#'
#' @seealso [chromoMap::chromoMap()] [karyoploteR::plotKaryotype()]
#' @keywords internal
#' @export
#' @examples
#' probes <- tibble::tribble(
#'   ~chrom, ~start, ~end, ~probe_set,
#'   "chr14", 2342135L, 2342284L, "IBC",
#'   "chr3", 830503L, 830769L, "DR2",
#'   "chr5", 482233L, 482391L, "IBC",
#'   "chr9", 375274L, 375417L, "IBC",
#'   "chr12", 532032L, 532281L, "DR2",
#'   "chr7", 383447L, 383653L, "HAP",
#'   "chr14", 1401991L, 1402160L, "IBC",
#'   "chr4", 734737L, 734936L, "HAP",
#'   "chr10", 93054L, 93223L, "IBC",
#'   "chr7", 162127L, 162277L, "IBC"
#' )
#' single_probe <- tibble::tribble(
#'   ~chrom, ~start, ~end, ~probe_set,
#'   "chr14", 2342135L, 2342284L, "IBC",
#'   "chr5", 482233L, 482391L, "IBC",
#'   "chr9", 375274L, 375417L, "IBC",
#'   "chr14", 1401991L, 1402160L, "IBC",
#'   "chr10", 93054L, 93223L, "IBC",
#'   "chr7", 162127L, 162277L, "IBC"
#' )
#'
#' chromosome_map(genome_Pf3D7, single_probe, "karyoploteR")
#' # ->
#' plot_karyoploteR(genome_Pf3D7, single_probe)
#'
#' chromosome_map(genome_Pf3D7, probes, "chromoMap")
#' # ->
#' plot_chromoMap(genome_Pf3D7, probes)
#'
#' chromosome_map(genome_Pf3D7, single_probe, "chromoMap", colours = "red")
#' # ->
#' plot_chromoMap(genome_Pf3D7, single_probe, colours = "red")
#'
#' chromosome_map(
#'   genome_Pf3D7,
#'   probes,
#'   "karyoploteR",
#'   title = "Example Chromosome Map",
#'   colours = c("#006A8EFF", "#A8A6A7FF", "#B1283AFF")
#' )
#' # ->
#' plot_karyoploteR(
#'   genome_Pf3D7,
#'   probes,
#'   title = "Example Chromosome Map",
#'   colours = c("#006A8EFF", "#A8A6A7FF", "#B1283AFF")
#' )
chromosome_map <- function(genome,
                           probes,
                           map_pkg = c("chromoMap", "karyoploteR"),
                           title = "",
                           colours = list(),
                           ...) {
  msg <- paste(
    "The function has been deprecated in favor of `plot_chromoMap()` and\n",
    "`plot_karyoploteR()`."
  )

  lifecycle::deprecate_warn(
    when = "0.2.0",
    what = "chromosome_map()",
    details = msg
  )

  # Check formatting of inputs
  if (ncol(genome) != 3) {
    cli_abort("Genomic information is misformatted.")
  }
  if (ncol(probes) != 4) {
    cli_abort(c(
      "Annotation information is misformatted.",
      "i" = "Did you forget to indicate the probe sets?"
    ))
  }

  # Call underlying implementation
  if (length(map_pkg) > 1) {
    cli_abort(c(
      "`map_pkg` must be of length 1.",
      i = '`map_pkg` must be either "chromoMap" or "karyoploteR".'
    ))
  } else if (map_pkg == "chromoMap") {
    plot_chromoMap(genome, probes, title, colours)
  } else if (map_pkg == "karyoploteR") {
    plot_karyoploteR(genome, probes, title, colours)
  } else {
    cli_abort(c(
      '`map_pkg` must be either "chromoMap" or "karyoploteR".',
      x = 'You\'ve input "{ map_pkg }".'
    ))
  }
}
