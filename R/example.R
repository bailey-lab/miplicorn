#------------------------------------------------
#' Get path to miplicorn examples
#'
#' miplicorn contains several example files in its 'inst/extdata' directory.
#' This function can be used to access file paths.
#'
#' @param path Name of file. If `NULL`, all example files will be listed.
#'
#' @aliases example
#' @export
#' @examples
#' # Get path to one example
#' miplicorn_example("reference_AA_table.csv")
#'
#' # List all available examples
#' miplicorn_example()
miplicorn_example <- function(path = NULL) {
  if (is.null(path)) {
    dir(system.file("extdata", package = "miplicorn"))
  } else {
    system.file("extdata", path, package = "miplicorn", mustWork = TRUE)
  }
}
