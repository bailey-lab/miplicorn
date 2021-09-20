#------------------------------------------------
#' Create annotated chromosome map
#'
#' Render an interactive graphics visualization of entire chromosomes or
#' chromosomal regions. Annotate multiple targeted regions to visualize probe
#' targets.
#'
#' @param genome A tibble indicating the starting and ending position of each
#'   chromosome. Contains three columns:
#'   \itemize{
#'     \item Name of the chromosome
#'     \item The starting position of the chromosome
#'     \item The ending position of the chromosome.
#'   }
#' @param probes A tibble indicating the starting and ending position of each
#'   probe. Contains five columns:
#'   \itemize{
#'     \item A unique identifier for the particular probe
#'     \item Name of the chromosome the probe is on
#'     \item The starting position of the probe
#'     \item The ending position of the probe
#'     \item An identifier indicating the probe set the probe belongs to.
#'   }
#' @param title The title of the plot.
#' @param colours A list of colours indicating the annotation colour for each
#'   probe set.
#' @param ... <[`dynamic-dots`][rlang::dyn-dots]> Additional arguments passed to
#'   [chromoMap::chromoMap()].
#'
#' @seealso [chromoMap::chromoMap()]
#' @export
#' @examples
#' probes <- tibble::tribble(
#'   ~rowid,  ~chrom,  ~start,    ~end, ~probe_set,
#'   4487L, "chr14", 2342135L, 2342284L,      "IBC",
#'   2813L,  "chr3",  830503L,  830769L,      "DR2",
#'   4673L,  "chr5",  482233L,  482391L,      "IBC",
#'   3337L,  "chr9",  375274L,  375417L,      "IBC",
#'   2449L, "chr12",  532032L,  532281L,      "DR2",
#'   1565L,  "chr7",  383447L,  383653L,      "HAP",
#'   3115L, "chr14", 1401991L, 1402160L,      "IBC",
#'   1446L,  "chr4",  734737L,  734936L,      "HAP",
#'   4555L, "chr10",   93054L,   93223L,      "IBC",
#'   3627L,  "chr7",  162127L,  162277L,      "IBC"
#' )
#'
#' chromosome_map(genome_Pf3D7, probes)
#'
#' chromosome_map(
#'   genome_Pf3D7,
#'   probes,
#'   title = "Example Chromosome Map",
#'   colours = list(c("#006A8EFF", "#A8A6A7FF", "#B1283AFF"))
#' )
chromosome_map <- function(genome,
                           probes,
                           title = "",
                           colours = list(),
                           ...) {
  # Write temp .txt files
  genome_path <- tempfile("genome.txt")
  probes_path <- tempfile("probes.txt")
  readr::write_tsv(genome, genome_path, col_names = FALSE)
  readr::write_tsv(probes, probes_path, col_names = FALSE)

  # Generate list of arguments and set default values
  arguments <- dots_list(
    ch.files = genome_path,
    data.files = probes_path,
    title = title,
    data_colors = colours,
    data_based_color_map = T,
    data_type = "categorical",
    canvas_width = 650,
    chr_length = 5,
    legend = T,
    lg_x = 100,
    lg_y = 250,
    ...,
    .homonyms = "last"
  )

  # Function call
  call <- call2(quote(chromoMap::chromoMap), !!!arguments)

  # Evaluate call
  print(quiet(eval(call)))

  # Delete temp files
  unlink(c(genome_path, probes_path))
}
