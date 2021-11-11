# Deprecated version of read_file. Used to read ref, alt, and cov tables
# together while specifying `chrom` or `gene`.
deprec_read_file <- function(file,
                             chrom,
                             gene,
                             name = c("ref_umi_count", "alt_umi_count", "coverage")) {
  # If the user does not want to filter out any data
  if (!is_present(chrom) & !is_present(gene)) {
    combined <- vroom::vroom(file, col_names = FALSE, show_col_types = FALSE) %>%
      # Take the transpose of our matrix, making rows columns and columns rows.
      # This will allows us to keep all the data in our .csv file.
      tibble::rownames_to_column() %>%
      tidyr::pivot_longer(-.data$rowname) %>%
      tidyr::pivot_wider(
        names_from = .data$rowname,
        values_from = .data$value
      ) %>%
      # Assign the column names of our tibble and clean them up
      dplyr::select(-.data$name) %>%
      janitor::row_to_names(1) %>%
      # Convert our data to a long format
      tidyr::pivot_longer(
        cols = -c(1:6),
        names_to = "sample",
        values_to = "value"
      ) %>%
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
  if (is_present(chrom) & !is_present(gene)) {
    # In this case, we want to filter by chromosome.
    # Get the header
    header <- suppressMessages(
      vroom::vroom(
        file,
        col_select = (1 | dplyr::contains(chrom)),
        n_max = 5, show_col_types = FALSE
      )
    )

    # Get the filtered data
    filtered <- suppressMessages(
      vroom::vroom(
        file,
        col_select = (1 | dplyr::contains(chrom)),
        show_col_types = FALSE
      )
    ) %>%
      dplyr::slice(6:dplyr::n())
  } else if (!is_present(chrom) & is_present(gene)) {
    # In this case, we want to filter by gene
    header <- suppressMessages(
      vroom::vroom(
        file,
        col_select = (1 | dplyr::contains(gene)),
        n_max = 4, skip = 1, show_col_types = FALSE
      )
    )

    filtered <- suppressMessages(
      vroom::vroom(
        file,
        col_select = (1 | dplyr::contains(gene)),
        skip = 1, show_col_types = FALSE
      )
    ) %>%
      dplyr::slice(5:dplyr::n())
  }

  # Transpose the header
  header_t <- header %>%
    tibble::rownames_to_column() %>%
    tidyr::pivot_longer(-.data$rowname) %>%
    tidyr::pivot_wider(
      names_from = .data$rowname,
      values_from = .data$value
    ) %>%
    janitor::row_to_names(1) %>%
    janitor::clean_names()

  # Transpose the filtered data
  filtered_t <- filtered %>%
    tibble::rownames_to_column() %>%
    tidyr::pivot_longer(
      -.data$rowname,
      values_transform = list(value = as.character)
    ) %>%
    tidyr::pivot_wider(
      names_from = .data$rowname,
      values_from = .data$value
    ) %>%
    janitor::row_to_names(1) %>%
    tidyr::pivot_longer(
      cols = -1,
      names_to = "sample",
      values_to = "value",
      values_transform = list(value = as.numeric)
    ) %>%
    janitor::clean_names()

  # Combine the two tibbles together
  combined <- dplyr::full_join(header_t, filtered_t) %>%
    dplyr::mutate(dplyr::across(
      where(is.character),
      ~ stringr::str_remove(., "\\.{3}.*")
    )) %>%
    dplyr::relocate(sample) %>%
    dplyr::rename({{ name }} := .data$value)

  return(combined)
}
