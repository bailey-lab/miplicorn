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
#' \dontrun{
#' # Pass chromosome names to select them
#' read(ref_file, alt_file, cov_file, chrom = "chr13")
#' read(ref_file, alt_file, cov_file, chrom = c("chr4", "chr13"))}
read <- function(
  reference_table,
  alternate_table,
  coverage_table,
  chrom = deprecated(),
  gene = deprecated()
) {
  # Deprecated chrom
  if (lifecycle::is_present(chrom)) {
    lifecycle::deprecate_warn(
      when = "0.1.0",
      what = "read(chrom)",
      details = "Please use the `...` argument instead to filter data."
    )
  }

  # Deprecated gene
  if (lifecycle::is_present(gene)) {
    lifecycle::deprecate_warn(
      when = "0.1.0",
      what = "read(gene)",
      details = "Please use the `...` argument instead to filter data."
    )
  }

  # Error message if multiple criteria selected
  if (lifecycle::is_present(chrom) && lifecycle::is_present(gene)) {
    message <- glue::glue(
      "Multiple filtering criteria selected:",
      "\n\u2139 Select only one piece of information to filter on."
    )
    stop(message, call. = FALSE)
  }

  # Read in the three tables
  reference_table <- deprec_read_file(reference_table, chrom, gene, "ref_umi_count")
  alternate_table <- deprec_read_file(alternate_table, chrom, gene, "alt_umi_count")
  coverage_table <- deprec_read_file(coverage_table, chrom, gene, "coverage")

  # Combine three tibbles together
  bind_table <- dplyr::full_join(reference_table, alternate_table) %>%
    dplyr::full_join(coverage_table) %>%
    dplyr::mutate(dplyr::across(.data$ref_umi_count:.data$coverage, as.numeric))
}

#' #------------------------------------------------
#' Read file
#'
#' Read file containing MIPtools' data table.
#'
#' @param .file The path to the file to be read.
#' @inheritParams read
#' @param .name The name of the value we are interested in.
#'
#' @return Parsed file.
#' @keywords internal
read_file <- function(.file, ..., .name) {
  ### Ideas is to model this function like dplyr::filter
  ### The dots will represent a logical argument that can be used for
  ### filtering data

  ### This may be a bit tricky, because we don't actually use filter, we
  ### use the values in dplyr::contains(). So what we really need is both
  ### the name of the column we are interested in and the value we want to
  ### filter our data by....

  ### Another strategy could be just feeding in a names list into the dots.
  ### For instance, read_file(file, c(chrom = "chr13), .name = coverage)
  ### If we want multiple filtering steps, we may need to apply some sort of
  ### map to manipulate the expressions
  dots <- enquos(..., .ignore_empty = "all")
  check_named(dots)

  # If the user does not want to filter out any data
  if (length(dots) == 0) {
    combined <- vroom::vroom(
      file,
      col_names = FALSE,
      show_col_types = FALSE
    ) %>%
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
      dplyr::rename({{ .name }} := .data$value)

    return(combined)
  }

  # From here on out, we deal with filtering data. The difference between
  # filtering by different objects is the location of this information in the
  # header. For instance, in the case of chromosome, the information is in the
  # 1st row, but for filtering by gene, the information is in a later row.
  # To determine how many lines to skip when reading in out data, we find the
  # filtering object's position in the header.
  full_header <- .file %>%
    vroom::vroom(col_names = FALSE, col_select = 1, n_max = 6) %>%
    tibble::deframe() %>%
    stringr::str_to_lower() %>%
    stringr::str_replace(" ", "_")

  filter <- names(dots[1]) %>%
    stringr::str_to_lower() %>%
    stringr::str_replace(" ", "_")

  filter_pos <- which(full_header == filter) - 1
  if (!is_double(filter_pos, n = 1L)) {
    abort(c(
      "Unable to filter data.",
      i = "Must supply one filtering condition.",
      x = pluralize("{length(pos)} condition{?s} supplied.")))
  }

  # Read in the header
  header <- suppressMessages(
    vroom::vroom(
      file = .file,
      col_select = (1 | !!dots[[1]]),
      n_max = length(full_header) - filter_pos,
      skip = filter_pos,
      show_col_types = FALSE
    )
  )

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

  # Read in the data
  data <- suppressMessages(
    vroom::vroom(
      file = .file,
      col_select = (1 | !!dots[[1]]),
      skip = filter_pos,
      show_col_types = FALSE
    )
  ) %>%
    dplyr::slice((length(full_header) + 1 - filter_pos):dplyr::n())

  # Transpose the filtered data
  data_t <- data %>%
    tibble::rownames_to_column() %>%
    tidyr::pivot_longer(-.data$rowname,
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
  combined <- dplyr::full_join(header_t, data_t) %>%
    dplyr::mutate(dplyr::across(
      where(is.character),
      ~ stringr::str_remove(., "\\.{3}.*")
    )) %>%
    dplyr::relocate(sample) %>%
    dplyr::rename({{ .name }} := .data$value)
}

check_named <- function(dots) {
  named <- have_name(dots)

  for (i in which(!named)) {
    quo <- dots[[i]]

    # only allow named logical vectors, anything else is suspicious
    expr <- quo_get_expr(quo)
    abort(c(
      glue("Problem with `read()` input `..{i}`."),
      x = glue("Input `..{i}` is unnamed."),
      i = glue("This usually means that you've used `==` instead of `=`."),
      i = glue("Faulty expression is `{as_label(expr)}`.")
    ))
  }
}

# Deprecated version of read_file
deprec_read_file <- function(
  file,
  chrom,
  gene,
  name = c("ref_umi_count", "alt_umi_count", "coverage")
) {
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
      vroom::vroom(file,
        col_select = (1 | dplyr::contains(chrom)),
        n_max = 5, show_col_types = FALSE
      )
    )

    # Get the filtered data
    filtered <- suppressMessages(
      vroom::vroom(file,
        col_select = (1 | dplyr::contains(chrom)),
        show_col_types = FALSE
      )
    ) %>%
      dplyr::slice(6:dplyr::n())
  } else if (!is_present(chrom) & is_present(gene)) {
    # In this case, we want to filter by gene
    header <- suppressMessages(
      vroom::vroom(file,
        col_select = (1 | dplyr::contains(gene)),
        n_max = 4, skip = 1, show_col_types = FALSE
      )
    )

    filtered <- suppressMessages(
      vroom::vroom(file,
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
    tidyr::pivot_longer(-.data$rowname,
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
