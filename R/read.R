#------------------------------------------------
#' Read data
#'
#' Read files containing MIPtools' data tables and combine them.
#'
#' @param reference_table File path to the reference table.
#' @param alternate_table File path to the alternate table.
#' @param coverage_table File path to the coverage table.
#' @param chrom The chromosome(s) to filter to.
#' @param gene The gene(s) to filter to.
#'
#' @return A tibble containing the parsed data.
#' @export
#' @examples
#' # Get path to example file
#' ref_file <- MIPr_example("reference_AA_table.csv")
#' alt_file <- MIPr_example("alternate_AA_table.csv")
#' cov_file <- MIPr_example("coverage_AA_table.csv")
#' ref_file
#'
#' # Input sources -------------------------------------------------------------
#' # Read from a path
#' read(ref_file, alt_file, cov_file)
#' # You can also use paths directly
#' # read("reference_AA_table.csv", "alternate_AA_table.csv", "coverage_AA_table.csv")
#'
#' # Read entire file ----------------------------------------------------------
#' read(ref_file, alt_file, cov_file)
#'
#' # Data selection ------------------------------------------------------------
#' # Pass gene names to select them
#' read(ref_file, alt_file, cov_file, gene = "atp6")
#' read(ref_file, alt_file, cov_file, gene = c("atp6", "crt"))
#'
#' \dontrun{
#' # Pass chromosome names to select them
#' read(ref_file, alt_file, cov_file, chrom = "chr13")
#' read(ref_file, alt_file, cov_file, chrom = c("chr4", "chr13"))}
read <- function(reference_table, alternate_table, coverage_table,
                 chrom = NULL, gene = NULL) {
  # Error message if multiple criteria selected
  if (!is.null(chrom) && !is.null(gene)) {
    message <- glue::glue("Multiple filtering criteria selected:",
                          '\n\u2139 Select only one piece of information to filter on.')
    stop(message, call. = FALSE)
  }

  # Read in the three tables
  reference_table <- read_file(reference_table, chrom, gene, "ref_umi_count")
  alternate_table <- read_file(alternate_table, chrom, gene, "alt_umi_count")
  coverage_table  <- read_file(coverage_table,  chrom, gene, "coverage")

  # Combine three tibbles together
  bind_table <- dplyr::full_join(reference_table, alternate_table) %>%
    dplyr::full_join(coverage_table) %>%
    dplyr::mutate(dplyr::across(.data$ref_umi_count:.data$coverage, as.numeric))

  return(bind_table)
}

#------------------------------------------------
#' Read file
#'
#' Read file containing MIPtools' data table.
#'
#' @param file The path to the file to be read.
#' @inheritParams read
#' @param name The name of the value we are interested in.
#'
#' @return Parsed file.
#' @keywords internal
read_file <- function(file,
                      chrom, gene,
                      name = c("ref_umi_count", "alt_umi_count", "coverage")) {
  # If the user does not want to filter out any data
  if (is.null(chrom) & is.null(gene)) {
    combined <- vroom::vroom(file, col_names = FALSE, show_col_types = FALSE) %>%
      # Take the transpose of our matrix, making rows columns and columns rows.
      # This will allows us to keep all the data in our .csv file.
      tibble::rownames_to_column() %>%
      tidyr::pivot_longer(-.data$rowname) %>%
      tidyr::pivot_wider(names_from = .data$rowname,
                         values_from = .data$value) %>%
      # Assign the column names of our tibble and clean them up
      dplyr::select(-.data$name) %>%
      janitor::row_to_names(1) %>%
      # Convert our data to a long format
      tidyr::pivot_longer(cols = -c(1:6),
                          names_to = "sample" ,
                          values_to = "value") %>%
      janitor::clean_names() %>%
      dplyr::relocate(sample) %>%
      dplyr::rename({{ name }} := .data$value)

    return(combined)
  }

  # From here on out, we deal with filtering data. The difference between
  # filtering by different objects is the location of this information in the
  # header. For instance, in the case of chromosome, the information is in the
  # 1st row, but for filtering by gene, the information is in a later row.
  # Depending on which row we want to look at, we must have different
  # expressions.
  if (!is.null(chrom) & is.null(gene)) {
    # In this case, we want to filter by chromosome.
    # Get the header
    header <- suppressMessages(
      vroom::vroom(file, col_select = (1 | dplyr::contains(chrom)),
                   n_max = 5, show_col_types = FALSE))

    # Get the filtered data
    filtered <- suppressMessages(
      vroom::vroom(file, col_select = (1 | dplyr::contains(chrom)),
                   show_col_types = FALSE)) %>%
      dplyr::slice(6:dplyr::n())

  } else if (is.null(chrom) & !is.null(gene)) {
    # In this case, we want to filter by gene
    header <- suppressMessages(
      vroom::vroom(file, col_select = (1 | dplyr::contains(gene)),
                   n_max = 4, skip = 1, show_col_types = FALSE))

    filtered <- suppressMessages(
      vroom::vroom(file, col_select = (1 | dplyr::contains(gene)),
                   skip = 1, show_col_types = FALSE)) %>%
      dplyr::slice(5:dplyr::n())
  }

  # Transpose the header
  header_t <- header %>%
    tibble::rownames_to_column() %>%
    tidyr::pivot_longer(-.data$rowname) %>%
    tidyr::pivot_wider(names_from = .data$rowname,
                       values_from = .data$value) %>%
    janitor::row_to_names(1) %>%
    janitor::clean_names()

  # Transpose the filtered data
  filtered_t <- filtered %>%
    tibble::rownames_to_column() %>%
    tidyr::pivot_longer(-.data$rowname,
                        values_transform = list(value = as.character)) %>%
    tidyr::pivot_wider(names_from = .data$rowname,
                       values_from = .data$value) %>%
    janitor::row_to_names(1) %>%
    tidyr::pivot_longer(cols = -1,
                        names_to = "sample" ,
                        values_to = "value",
                        values_transform = list(value = as.numeric)) %>%
    janitor::clean_names()

  # Combine the two tibbles together
  combined <- dplyr::full_join(header_t, filtered_t) %>%
    dplyr::mutate(dplyr::across(where(is.character),
                                ~stringr::str_remove(., "\\.{3}.*"))) %>%
    dplyr::relocate(sample) %>%
    dplyr::rename({{ name }} := .data$value)

  return(combined)
}
