#------------------------------------------------
#' Create annotated chromosome map
#'
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
#' @param title The title of the plot.
#' @param colours A vector of colours indicating the annotation colour for each
#'   probe set.
#' @param ... <[`dynamic-dots`][rlang::dyn-dots]> Additional arguments passed to
#'   internal plotting functions.
#'
#' @seealso [chromoMap::chromoMap()] [karyoploteR::plotKaryotype()]
#' @name chromosome-map
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
#' plot_chromoMap(genome_Pf3D7, probes)
#' plot_chromoMap(genome_Pf3D7, single_probe, colours = "red")
#'
#' plot_karyoploteR(genome_Pf3D7, single_probe)
#' plot_karyoploteR(
#'   genome_Pf3D7,
#'   probes,
#'   title = "Example Chromosome Map",
#'   colours = c("#006A8EFF", "#A8A6A7FF", "#B1283AFF")
#' )
NULL

#' @rdname chromosome-map
#' @export
plot_chromoMap <- function(genome,
                           probes,
                           title = "",
                           colours = list(),
                           ...) {
  # Ensure packages installed
  if (!requireNamespace("chromoMap", quietly = TRUE) |
    !requireNamespace("withr", quietly = TRUE)) {
    abort('Packages "chromoMap" and "withr" needed to create chromosome maps. Please install them.')
  }

  # Check inputs
  check_inputs(genome, probes)

  # Add unique id to probes
  probes <- tibble::rowid_to_column(probes)

  # Write temp .txt files
  tempdir <- withr::local_tempdir()
  genome_path <- withr::local_tempfile(
    pattern = "genome",
    fileext = ".txt",
    tmpdir = tempdir
  )
  probes_path <- withr::local_tempfile(
    pattern = "probes",
    fileext = ".txt",
    tmpdir = tempdir
  )
  vroom::vroom_write(genome, genome_path, col_names = FALSE)
  vroom::vroom_write(probes, probes_path, col_names = FALSE)

  # Determine whether there is one or more probe_sets and assign colors
  # accordingly.
  n_probe_sets <- dplyr::n_distinct(probes[, 5])
  if (n_probe_sets == 1) {
    data_based_color_map <- F
    anno_col <- if (rlang::is_empty(colours)) "#A8A6A7FF" else colours
  } else {
    data_based_color_map <- T
    anno_col <- "#A8A6A7FF"
    data_colors <- colours
  }

  # Generate list of arguments and set default values
  arguments <- rlang::dots_list(
    ch.files = genome_path,
    data.files = probes_path,
    title = title,
    data_based_color_map = data_based_color_map,
    anno_col = anno_col,
    data_colors = if (rlang::is_list(colours)) colours else list(colours),
    data_type = "categorical",
    segment_annotation = T,
    canvas_width = 650,
    chr_length = 5,
    legend = T,
    lg_x = 100,
    lg_y = 250,
    ...,
    .homonyms = "last"
  )

  # Execute function
  print(quiet(rlang::exec(rlang::expr(chromoMap::chromoMap), !!!arguments)))
}

#' @rdname chromosome-map
#' @export
plot_karyoploteR <- function(genome,
                             probes,
                             title = "",
                             colours = list(),
                             ...) {
  # Ensure packages installed
  if (!requireNamespace("karyoploteR", quietly = TRUE)) {
    abort('Package "karyoploteR" needed to create chromosome maps. Please install it.')
  }

  # Check inputs
  check_inputs(genome, probes)

  genome <- genome %>%
    data.frame() %>%
    regioneR::toGRanges()

  arguments <- rlang::dots_list(
    # Default plot params
    data1height = 200,
    data1outmargin = 70,
    bottommargin = 150,
    topmargin = 150,

    # Default base plot params
    plot.type = 1,
    labels.plotter = NULL,
    main = title,

    # Default tick marks params
    tick.dist = 500000,
    minor.tick.dist = 100000,
    add.units = TRUE,
    minor.ticks = TRUE,
    tick.len = 10,
    minor.tick.len = 5,
    minor.tick.col = "black",

    # Default chrom color
    color.schema = "biovizbase",

    # Default chrom label size
    cex = 0.8,

    # Other parameters and options
    ...,
    .homonyms = "last"
  )

  # Modify plotting params
  plot_params <- karyoploteR::getDefaultPlotParams(plot.type = 1)
  to_modify <- arguments %>% purrr::keep(names(.) %in% names(plot_params))
  plot_params <- purrr::list_modify(plot_params, !!!to_modify)

  # Create base plot
  base <- rlang::exec(
    rlang::expr(karyoploteR::plotKaryotype),
    genome = genome,
    plot.params = plot_params,
    !!!extract_args(arguments, karyoploteR::plotKaryotype)
  ) %>%
    add_base_layer(karyoploteR::kpAddBaseNumbers, arguments) %>%
    add_base_layer(karyoploteR::kpAddCytobands, arguments) %>%
    add_base_layer(karyoploteR::kpAddChromosomeNames, arguments)

  # Find the number of probe sets
  probe_sets <- dplyr::distinct(probes[, 4])
  n_probe_sets <- nrow(probe_sets)

  # Create list of plotting data
  plot_data <- purrr::map(
    purrr::as_vector(probe_sets),
    function(x) {
      probes %>%
        dplyr::filter(probes[, 4] == {{ x }}) %>%
        data.frame() %>%
        regioneR::toGRanges()
    }
  )

  # Create list of plotting areas
  plot_areas <- purrr::map(
    seq_len(n_probe_sets),
    karyoploteR::autotrack,
    n_probe_sets
  ) %>%
    purrr::transpose()

  # Default colour
  colours <- if (rlang::is_empty(colours)) "black" else colours

  # Add each probe set to plot
  purrr::pwalk(
    list(
      data = plot_data,
      r0 = plot_areas$r0,
      r1 = plot_areas$r1,
      col = colours
    ),
    add_data_layer,
    karyoplot = base
  )

  # Add legend
  graphics::legend(
    x = "right",
    fill = colours,
    legend = purrr::as_vector(probe_sets)
  )
}

# Extract arguments of a function from a list
extract_args <- function(list, fn) {
  list %>%
    purrr::keep(names(.) %in% rlang::fn_fmls_names(fn))
}

# Add a base layer to the karyoplot
add_base_layer <- function(karyoplot, fn, arguments) {
  rlang::exec(
    rlang::expr(fn),
    karyoplot = karyoplot,
    !!!extract_args(arguments, fn)
  )
}

# Add a data layer to the karyoplot
add_data_layer <- function(karyoplot, data, r0, r1, col) {
  rlang::exec(
    rlang::expr(karyoploteR::kpPlotRegions),
    karyoplot = karyoplot,
    data = data,
    r0 = r0,
    r1 = r1,
    avoid.overlapping = FALSE,
    col = col
  )
}

# To silence the R CMD Check
globalVariables(".")

# Check formatting of inputs
check_inputs <- function(genome, probes) {
  if (ncol(genome) != 3) {
    abort("Genomic information is misformatted.")
  }
  if (ncol(probes) != 4) {
    abort(c(
      "Annotation information is misformatted.",
      "i" = "Did you forget to indicate the probe sets?"
    ))
  }
}
