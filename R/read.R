#------------------------------------------------
#' Read data
#'
#' Read files containing MIPtools' data tables and combine them.
#'
#' @param reference_table File path to the reference table.
#' @param alternate_table File path to the alternate table.
#' @param coverage_table File path to the coverage table.
#'
#' @return A tibble containing the parsed data.
#' @export
read <- function(reference_table, alternate_table, coverage_table) {
  reference_table <- read_file(reference_table, "reference")
  alternate_table <- read_file(alternate_table, "alternate")
  coverage_table  <- read_file(coverage_table, "coverage")

  bind_table <- dplyr::full_join(reference_table, alternate_table) %>%
    dplyr::full_join(coverage_table) %>%
    dplyr::mutate(dplyr::across(.data$ref_umi_count:.data$coverage, as.numeric))
}

#------------------------------------------------
#' Read file
#'
#' Read file containing MIPtools' data table.
#'
#' @param file The path to the file to be read.
#' @param type Whether the table is an alternate or coverage table.
#'
#' @return Parsed file.
#' @keywords internal
read_file <- function(file, type = c("reference", "alternate", "coverage")) {
  data <- vroom::vroom(file, col_names = FALSE, show_col_types = FALSE) %>%
    # Take the transpose of our matrix, making rows columns and columns rows.
    # This will allows us to keep all the data in our .csv file.
    tibble::rownames_to_column() %>%
    tidyr::pivot_longer(-.data$rowname) %>%
    tidyr::pivot_wider(names_from = .data$rowname,
                       values_from = .data$value) %>%
    # Assign the column names of our tibble and clean them up
    dplyr::select(-.data$name) %>%
    janitor::row_to_names(1)
  if (type == "reference") {
    data <- data %>%
      # Convert out data to a long format, where now we have a column with the
      # ref_umi_count
      tidyr::pivot_longer(cols = -c(1:6),
                          names_to = "sample" ,
                          values_to = "ref_umi_count")
  } else if (type == "alternate") {
    data <- data %>%
      # Convert out data to a long format, where now we have a column with the
      # alt_umi_count
      tidyr::pivot_longer(cols = -c(1:6),
                          names_to = "sample" ,
                          values_to = "alt_umi_count")
  } else if (type == "coverage") {
    data <- data %>%
      # Convert out data to a long format, where now we have a column with the
      # coverage
      tidyr::pivot_longer(cols = -c(1:6),
                          names_to = "sample" ,
                          values_to = "coverage")
  } else {
    stop("Inocrrect type specified")
  }
  data <- data %>%
    dplyr::relocate(sample) %>%
    janitor::clean_names()
}
