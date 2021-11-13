#------------------------------------------------
#' Read data
#'
#' @description
#' `r lifecycle::badge("deprecated")`
#'
#' `read_file()` has been replaced by `read_tbl_reference()`,
#' `read_tbl_alternate()`, and `read_tbl_coverage()` to provide more specific
#' functionality.
#'
#' `read()` has been renamed to `read_tbl_ref_alt_cov()`.
#'
#' @details
#' Read files containing
#' \href{https://github.com/bailey-lab/MIPTools}{MIPtools'} data tables.
#' `read_file()` reads a single file. `read()` is a convenience function that
#' reads all files output by
#' \href{https://github.com/bailey-lab/MIPTools}{MIPtools} and combines them.
#' Data files include the reference table, the alternate table, and the coverage
#' table. Data is read lazily using the
#' \href{https://vroom.r-lib.org/index.html}{`vroom`} package. Data can be
#' filtered, retaining all rows that satisfy the conditions. To be retained, the
#' row in question must produce a value of `TRUE` for all conditions. Note that
#' when a condition evaluates to NA, the row will be dropped.
#'
#' @section Data structure:
#' Input data must contain six rows of metadata. The metadata can vary depending
#' on what type of file is read, but typically contains information about the
#' location of a mutation. The remaining rows represent the data for each sample
#' sequenced. Together, the alternate, reference, and coverage tables can
#' provide information about mutations observed and the coverage at each site.
#'
#' @section Useful filter functions:
#' The [dplyr::filter()] function is employed to subset the rows of the data
#' applying the expressions in `...` to the column values to determine which
#' rows should be retained.
#'
#' There are many functions and operators that are useful when constructing the
#' expressions used to filter the data:
#'
#' * [`==`], [`>`], [`>=`], etc.
#' * [`&`], [`|`], [`!`], [xor()]
#' * [is.na()]
#' * [`between()`][dplyr::between()], [`near()`][dplyr::near()]
#'
#' @param .ref_file File path to the reference table.
#' @param .alt_file File path to the alternate table.
#' @param .cov_file File path to the coverage table.
#' @param ...
#'   <[`data-masking`](https://dplyr.tidyverse.org/reference/dplyr_data_masking.html)>
#'   Expressions that return a logical value and are used to filter the data. If
#'   multiple expressions are included, they are combined with the `&` operator.
#'   Only rows for which all conditions evaluate to `TRUE` are kept.
#' @param chrom `r lifecycle::badge("deprecated")`: The chromosome(s) to filter
#'   to.
#' @param gene `r lifecycle::badge("deprecated")`: The gene(s) to filter to.
#' @param .file File path to a file.
#' @param .name The information contained in the specific file. For example
#'   `"coverage"` or `"ref_umi_count"`.
#'
#' @return A [`tibble()`][tibble::tibble()]. The first six columns contain the
#'   metadata associated with each sample and mutation. Columns `ref_umi_count`
#'   and `alt_umi_count` contain the umi count of the reference and alternate
#'   allele, respectively. Column `coverage` contains the coverage for each data
#'   point.
#'
#' @seealso [vroom::vroom()] [dplyr::filter()]
#' @keywords internal
#' @examples
#' # Get path to example file
#' ref_file <- miplicorn_example("reference_AA_table.csv")
#' alt_file <- miplicorn_example("alternate_AA_table.csv")
#' cov_file <- miplicorn_example("coverage_AA_table.csv")
#' cov_file
#'
#' # Input sources -------------------------------------------------------------
#' # Read from a path
#' read_file(cov_file, .name = "coverage")
#' read(ref_file, alt_file, cov_file)
#'
#' # You can also use paths directly
#' # read_file("reference_AA_table.csv")
#' # read("reference_AA_table.csv", "alternate_AA_table.csv", "coverage_AA_table.csv")
#'
#' # Read entire file ----------------------------------------------------------
#' read_file(cov_file, .name = "coverage")
#' read(ref_file, alt_file, cov_file)
#'
#' # Data filtering ------------------------------------------------------------
#' # Filtering by one criterion
#' read_file(cov_file, gene == "atp6", .name = "coverage")
#' read(ref_file, alt_file, cov_file, gene == "atp6")
#'
#' # Filtering by multiple criteria within a single logical expression
#' read_file(cov_file, gene == "atp6" & targeted == "Yes", .name = "coverage")
#' read_file(cov_file, gene == "atp6" | targeted == "Yes", .name = "coverage")
#' read(ref_file, alt_file, cov_file, gene == "atp6" & targeted == "Yes")
#' read(ref_file, alt_file, cov_file, gene == "atp6" | targeted == "Yes")
#'
#' # When multiple expressions are used, they are combined using &
#' read_file(cov_file, gene == "atp6", targeted == "Yes", .name = "coverage")
#' read(ref_file, alt_file, cov_file, gene == "atp6", targeted == "Yes")
#' @name read-deprecated
NULL

#' @rdname read-deprecated
#' @export
read <- function(.ref_file,
                 .alt_file,
                 .cov_file,
                 ...,
                 chrom = deprecated(),
                 gene = deprecated()) {
  lifecycle::deprecate_warn(
    when = "0.2.0",
    what = "read()",
    with = "read_tbl_ref_alt_cov()"
  )

  read_tbl_ref_alt_cov(
    .tbl_ref = .ref_file,
    .tbl_alt = .alt_file,
    .tbl_cov = .cov_file,
    ...,
    chrom = chrom,
    gene = gene
  )
}

#' @rdname read-deprecated
#' @export
read_file <- function(.file, ..., .name = "value") {
  msg <- paste(
    "The function has been replaced by three more specific functions:\n",
    "`read_tbl_reference()`, `read_tbl_alternate()`, and",
    "`read_tbl_coverage()`."
  )

  lifecycle::deprecate_warn(
    when = "0.2.0",
    what = "read_file()",
    details = msg
  )

  # Detect input table and pass on to functions
  if (.name == "ref_umi_count") {
    inform("Input detected as the reference table.")
    read_tbl_reference(.file, ...)
  } else if (.name == "alt_umi_count") {
    inform("Input detected as the alternate table.")
    read_tbl_alternate(.file, ...)
  } else if (.name == "coverage") {
    inform("Input detected as the coverage table.")
    read_tbl_coverage(.file, ...)
  } else {
    abort("Unable to detect the type of table input.")
  }
}

# Deprecated version of read_file. Used to read ref, alt, and cov tables
# together while specifying `chrom` or `gene`.
deprec_read_file <- function(file,
                             chrom,
                             gene,
                             name = c("ref_umi_count", "alt_umi_count", "coverage")) {
  # If the user does not want to filter out any data
  if (!is_present(chrom) & !is_present(gene)) {
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
        n_max = 5,
        show_col_types = FALSE
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
        n_max = 4,
        skip = 1,
        show_col_types = FALSE
      )
    )

    filtered <- suppressMessages(
      vroom::vroom(
        file,
        col_select = (1 | dplyr::contains(gene)),
        skip = 1,
        show_col_types = FALSE
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
