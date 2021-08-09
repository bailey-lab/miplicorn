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
  alternate_table <- read_file(alternate_table, "alternate")
  coverage_table  <- read_file(coverage_table, "coverage")

  bind_table <- dplyr::full_join(alternate_table, coverage_table,
                                    by = c("sample", "gene_id", "gene",
                                           "mutation_name", "exonic_func",
                                           "aa_change", "targeted")) %>%
    dplyr::mutate(dplyr::across(.data$umi_count:.data$coverage, as.numeric))
}

#------------------------------------------------
#' Read file
#'
#' Read data files.
#'
#' @param file Alternate table.
#'
#' @return Parsed file.
#'
#' @keywords internal
read_file <- function(file, type = c("alternate", "coverage")) {
  data <- vroom::vroom(file, col_names = FALSE, show_col_types = FALSE) %>%
    # Take the tranpose of our matrix, making rows columns and columns rows. This
    # will allows us to keep all the data in our .csv file.
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
      tidyr::pivot_longer(cols = -c(.data$`Gene ID`:.data$Targeted),
                          names_to = "sample" ,
                          values_to = "umi_count")
  } else if (type == "coverage") {
    data <- data %>%
      # Convert out data to a long format, where now we have a column with the
      # coverage
      tidyr::pivot_longer(cols = -c(.data$`Gene ID`:.data$Targeted),
                          names_to = "sample" ,
                          values_to = "coverage")
  } else {
    stop("Inocrrect type specified")
  }
  data <- data %>%
    dplyr::relocate(sample) %>%
    janitor::clean_names()
}
