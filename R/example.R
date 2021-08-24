#------------------------------------------------
#' Get path to MIPr examples
#'
#' MIPr contains several example files in its 'inst/extdata' directory. Use
#' `MIPr_example()` to retrieve the path to one file and `MIPr_examples()` to
#' list all the available files.
#'
#' Thanks to Jim Hester's and Hadley Wickham's \code{\link[vroom]{vroom}}
#' package for these functions.
#'
#' @param path Name of file.
#' @param pattern A regular expression of filenames to match. If `NULL` all
#' available files are listed.
#'
#' @aliases example examples
#' @export
#' @examples
#' # Get path to one example
#' MIPr_example("reference_AA_table.csv")
#'
#' # List all available examples
#' MIPr_examples()
MIPr_example <- function (path) {
  system.file("extdata", path, package = "MIPr", mustWork = TRUE)
}

#' @rdname MIPr_example
#' @export
MIPr_examples <- function (pattern = NULL) {
  list.files(system.file("extdata", package = "MIPr"), pattern = pattern)
}
