#------------------------------------------------
#' Read data
#'
#' Read paired data files and combine the files.
#'
#' @param alternate_table Alternate table.
#' @param coverage_table Coverage table.
#'
#' @return A tibble containing the parsed data.
#' @export
read <- function(alternate_table, coverage_table) {
  alternate_table <- read_table(alternate_table, "alternate")
  coverage_table  <- read_table(coverage_table, "coverage")

  bind_table <- dplyr::full_join(alternate_table, coverage_table) %>%
    dplyr::mutate(dplyr::across(.data$umi_count:.data$coverage, as.numeric))
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
#'
read_table <- function(file, type = c("alternate", "coverage")) {
#' @keywords internal
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
  if (type == "alternate") {
    data <- data %>%
      # Convert out data to a long format, where now we have a column with the
      # umi_count
      tidyr::pivot_longer(cols = -c(1:6),
                          names_to = "sample" ,
                          values_to = "umi_count")
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
