# Check if file is empty
empty_file <- function(x) {
  is.character(x) &&
    file.exists(x) &&
    file.size(x) == 0
}

# Hide text output
quiet <- function(x) {
  sink(nullfile())
  on.exit(sink())
  invisible(force(x))
}
