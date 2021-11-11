#------------------------------------------------
#' Read data
#'
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
#' The `dplyr::filter()` function is employed to subset the rows of the data
#' applying the expressions in `...` to the column values to determine which
#' rows should be retained.
#'
#' There are many functions and operators that are useful when constructing the
#' expressions used to filter the data:
#'
#' * [`==`], [`>`], [`>=`], etc.
#' * [`&`], [`|`], [`!`], [xor()]
#' * [is.na()]
#' * [between()], [near()]
#'
#' @param .ref_file File path to the reference table.
#' @param .alt_file File path to the alternate table.
#' @param .cov_file File path to the coverage table.
#' @param ... <[`data-masking`][dplyr_data_masking]> Expressions that return a
#'   logical value and are used to filter the data. If multiple expressions are
#'   included, they are combined with the `&` operator. Only rows for which all
#'   conditions evaluate to `TRUE` are kept.
#' @param chrom `r lifecycle::badge("deprecated")`: The chromosome(s) to filter
#'   to.
#' @param gene `r lifecycle::badge("deprecated")`: The gene(s) to filter to.
#' @param .file File path to a file.
#' @param .name The information contained in the specific file. For example
#'   `"coverage"` or `"ref_umi_count"`.
#'
#' @return A [tibble()]. The first six columns contain the metadata associated
#' with each sample and mutation. Columns `ref_umi_count` and `alt_umi_count`
#' contain the umi count of the reference and alternate allele, respectively.
#' Column `coverage` contains the coverage for each data point.
#'
#' @seealso [vroom::vroom()] [dplyr::filter()]
#' @export
#' @examples
#' # Get path to example file
#' ref_file <- miplicorn_example("reference_AA_table.csv")
#' alt_file <- miplicorn_example("alternate_AA_table.csv")
#' cov_file <- miplicorn_example("coverage_AA_table.csv")
#' ref_file
#'
#' # Input sources -------------------------------------------------------------
#' # Read from a path
#' read_file(ref_file, .name = "umi")
#' read(ref_file, alt_file, cov_file)
#'
#' # You can also use paths directly
#' # read_file("reference_AA_table.csv")
#' # read("reference_AA_table.csv", "alternate_AA_table.csv", "coverage_AA_table.csv")
#'
#' # Read entire file ----------------------------------------------------------
#' read_file(ref_file, .name = "umi")
#' read(ref_file, alt_file, cov_file)
#'
#' # Data filtering ------------------------------------------------------------
#' # Filtering by one criterion
#' read_file(ref_file, gene == "atp6", .name = "umi")
#' read(ref_file, alt_file, cov_file, gene == "atp6")
#'
#' # Filtering by multiple criteria within a single logical expression
#' read_file(ref_file, gene == "atp6" & targeted == "Yes", .name = "umi")
#' read_file(ref_file, gene == "atp6" | targeted == "Yes", .name = "umi")
#' read(ref_file, alt_file, cov_file, gene == "atp6" & targeted == "Yes")
#' read(ref_file, alt_file, cov_file, gene == "atp6" | targeted == "Yes")
#'
#' # When multiple expressions are used, they are combined using &
#' read_file(ref_file, gene == "atp6", targeted == "Yes", .name = "umi")
#' read(ref_file, alt_file, cov_file, gene == "atp6", targeted == "Yes")
read <- function(.ref_file,
                 .alt_file,
                 .cov_file,
                 ...,
                 chrom = deprecated(),
                 gene = deprecated()) {
  # Deprecated chrom
  if (lifecycle::is_present(chrom)) {
    lifecycle::deprecate_warn(
      when = "0.0.0.9001",
      what = "read(chrom)",
      details = "Please use the `...` argument instead to filter data."
    )
  }

  # Deprecated gene
  if (lifecycle::is_present(gene)) {
    lifecycle::deprecate_warn(
      when = "0.0.0.9001",
      what = "read(gene)",
      details = "Please use the `...` argument instead to filter data."
    )
  }

  # Error message if multiple criteria selected
  if (lifecycle::is_present(chrom) && lifecycle::is_present(gene)) {
    abort(c(
      "Multiple filtering criteria selected.",
      x = "Cannot filter on both `chrom` and `gene`.",
      i = "Select only one piece of information to filter on."
    ))
  }

  # Error if any file is empty
  if (purrr::some(list(.ref_file, .alt_file, .cov_file), empty_file)) {
    empty <- purrr::detect(list(.ref_file, .alt_file, .cov_file), empty_file)
    abort(c(
      "Unable to read files.",
      x = glue('"{empty}" is an empty file.')
    ))
  }

  # Read in the three files
  if (lifecycle::is_present(chrom) || lifecycle::is_present(gene)) {
    tables <- purrr::pmap(
      list(
        file = c(.ref_file, .alt_file, .cov_file),
        name = c("ref_umi_count", "alt_umi_count", "coverage")
      ),
      deprec_read_file,
      chrom = chrom,
      gene = gene
    )
  } else {
    tables <- purrr::pmap(
      list(
        .file = c(.ref_file, .alt_file, .cov_file),
        .name = c("ref_umi_count", "alt_umi_count", "coverage")
      ),
      read_file,
      ...
    )
  }

  # Determine overlapping columns
  by <- purrr::reduce(purrr::map(tables, colnames), intersect)

  # Combine three tibbles together
  purrr::reduce(tables, dplyr::full_join, by = by)
}

#' @rdname read
#' @export
read_tbl_reference <- function(.file, ...) {
  read_file(.file, ..., .name = "ref_umi_count")
}

#' @export
read_tbl_alternate <- function(.file, ...) {
  read_file(.file, ..., .name = "alt_umi_count")
}

#' @export
read_tbl_coverage <- function(.file, ...) {
  read_file(.file, ..., .name = "coverage")
}

read_file <- function(.file, ..., .name = "value") {
  dots <- enquos(..., .ignore_empty = "all")
  check_named(dots)

  if (empty_file(.file)) {
    return(tibble::tibble())
  }

  # Read in complete header
  header <- .file %>%
    vroom::vroom(col_names = FALSE, show_col_types = FALSE, n_max = 6) %>%
    tibble::rownames_to_column() %>%
    tidyr::pivot_longer(-.data$rowname) %>%
    tidyr::pivot_wider(
      names_from = .data$rowname,
      values_from = .data$value
    ) %>%
    janitor::row_to_names(1) %>%
    janitor::clean_names()

  # Filter the header based on conditions specified
  tryCatch(
    filter_header <- dplyr::filter(header, ...),
    error = function(e) {
      e <- rlang::catch_cnd(dplyr::filter(header, ...))
      msg <- e$message %>%
        stringr::str_replace("filter", "read") %>%
        stringr::str_replace("object", "Object") %>%
        stringr::str_c(".")
      objects <- stringr::str_c("'", colnames(header)[-1], "'")
      abort(c(
        msg,
        i = cli::pluralize("Available objects are {objects}.")
      ))
    }
  )

  # Extract which columns of data we are interested in
  col_select <- filter_header[[1]] %>%
    stringr::str_extract("\\d+") %>%
    as.numeric()

  # Read in entire data set but select only columns we are interested in
  data <- vroom::vroom(
    file = .file,
    col_names = FALSE,
    col_select = c(1, col_select),
    show_col_types = FALSE,
    .name_repair = "universal"
  )

  # Take the transpose of our matrix, making rows columns and columns rows
  t_data <- data %>%
    tibble::rownames_to_column() %>%
    tidyr::pivot_longer(-.data$rowname) %>%
    tidyr::pivot_wider(
      names_from = .data$rowname,
      values_from = .data$value
    ) %>%
    # Assign the column names of our tibble and clean them up
    dplyr::select(-.data$name) %>%
    janitor::row_to_names(1) %>%
    janitor::clean_names()

  # Convert our data to a long format
  t_data %>%
    tidyr::pivot_longer(
      cols = -c(1:6),
      names_to = "sample",
      values_to = "value"
    ) %>%
    dplyr::relocate(sample) %>%
    dplyr::mutate(value = as.numeric(.data$value)) %>%
    dplyr::rename({{ .name }} := .data$value)
}

check_named <- function(dots) {
  named <- rlang::have_name(dots)

  for (i in which(named)) {
    quo <- dots[[i]]

    # only allow unnamed logical vectors, anything else is suspicious
    expr <- rlang::quo_get_expr(quo)
    if (!rlang::is_logical(expr)) {
      name <- names(dots)[i]
      abort(c(
        glue("Problem with `read()` input `..{i}`."),
        x = glue("Input `..{i}` is named"),
        i = "This usually means that you've used `=` instead of `==`.",
        i = glue("Did you mean `{name} == {as_label(expr)}`?")
      ))
    }
  }
}
