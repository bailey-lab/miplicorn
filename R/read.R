#------------------------------------------------
#' Read data
#'
#' Read files containing MIPtools' data tables and combine them.
#'
#' @param reference_table File path to the reference table.
#' @param alternate_table File path to the alternate table.
#' @param coverage_table File path to the coverage table.
#' @param chrom The chromosome(s) to filter to.
#'
#' @return A tibble containing the parsed data.
#' @export
read <- function(reference_table, alternate_table, coverage_table,
                 chrom = NULL) {
  # Read in the three tables
  reference_table <- read_file(reference_table, chrom, "ref_umi_count")
  alternate_table <- read_file(alternate_table, chrom, "alt_umi_count")
  coverage_table  <- read_file(coverage_table,  chrom, "coverage")

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
#' @param chrom The chromosome(s) to filter to.
#' @param name The name of the value we are interested in.
#'
#' @return Parsed file.
#' @keywords internal
read_file <- function(file,
                      chrom,
                      name = c("ref_umi_count", "alt_umi_count", "coverage")) {
  if (is.null(chrom)) {
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
      janitor::clean_names()
  } else {
    # Get the header, which includes info like the chromosome and position
    header <- suppressMessages(
      vroom::vroom(file, col_select = (.data$CHROM | dplyr::contains(chrom)),
                   n_max = 5, show_col_types = FALSE))

    # Transpose the header
    header_t <- header %>%
      tibble::rownames_to_column() %>%
      tidyr::pivot_longer(-.data$rowname) %>%
      tidyr::pivot_wider(names_from = .data$rowname,
                         values_from = .data$value) %>%
      janitor::row_to_names(1) %>%
      janitor::clean_names()

    # Get the filtered data
    filtered <- suppressMessages(
      vroom::vroom(file, col_select = (.data$CHROM | dplyr::contains(chrom)),
                   show_col_types = FALSE)) %>%
      dplyr::slice(6:dplyr::n())

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
      dplyr::mutate(chrom = stringr::str_replace(chrom, "\\..*", ""))
  }

  # Clean final dataset
  final_data <- combined %>%
    dplyr::relocate(sample) %>%
    dplyr::rename({{ name }} := .data$value)

  return(final_data)
}
