#------------------------------------------------
#' Get path to miplicorn examples
#'
#' miplicorn contains several example files in its 'inst/extdata' directory. Use
#' `miplicorn_example()` to retrieve the path to one file and
#' `miplicorn_examples()` to list all the available files.
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
#' miplicorn_example("reference_AA_table.csv")
#'
#' # List all available examples
#' miplicorn_examples()
miplicorn_example <- function(path) {
  system.file("extdata", path, package = "miplicorn", mustWork = TRUE)
}

#' @rdname miplicorn_example
#' @export
miplicorn_examples <- function(pattern = NULL) {
  list.files(system.file("extdata", package = "miplicorn"), pattern = pattern)
}
