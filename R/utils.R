empty_file <- function(x) {
  is.character(x) &&
    file.exists(x) &&
    file.info(x, extra_cols = FALSE)$size == 0
}

# Hide text output
quiet <- function(x) {
  sink(tempfile())
  on.exit(sink())
  invisible(force(x))
}
