# Reference table --------------------------------------------------------------
new_ref_tbl <- function(x) {
  tibble::new_tibble(x, class = "ref_tbl")
}

#' @export
`[.ref_tbl` <- function(x, i, j, drop = FALSE) {
  ref_tbl_reconstruct(NextMethod())
}

#' @export
`names<-.ref_tbl` <- function(x, value) {
  ref_tbl_reconstruct(NextMethod())
}

#' @export
`$<-.ref_tbl` <- function(x, name, value) {
  ref_tbl_reconstruct(NextMethod())
}

# Alternate table --------------------------------------------------------------
new_alt_tbl <- function(x) {
  tibble::new_tibble(x, class = "alt_tbl")
}

#' @export
`[.alt_tbl` <- function(x, i, j, drop = FALSE) {
  alt_tbl_reconstruct(NextMethod())
}

#' @export
`names<-.alt_tbl` <- function(x, value) {
  alt_tbl_reconstruct(NextMethod())
}

#' @export
`$<-.alt_tbl` <- function(x, name, value) {
  alt_tbl_reconstruct(NextMethod())
}

# Genotype table ---------------------------------------------------------------
new_geno_tbl <- function(x) {
  tibble::new_tibble(x, class = "geno_tbl")
}

#' @export
`[.geno_tbl` <- function(x, i, j, drop = FALSE) {
  geno_tbl_reconstruct(NextMethod())
}

#' @export
`names<-.geno_tbl` <- function(x, value) {
  geno_tbl_reconstruct(NextMethod())
}

#' @export
`$<-.geno_tbl` <- function(x, name, value) {
  geno_tbl_reconstruct(NextMethod())
}

# Reference, alternate, coverage table -----------------------------------------
new_ref_alt_cov_tbl <- function(x) {
  tibble::new_tibble(x, class = "ref_alt_cov_tbl")
}

#' @export
`[.ref_alt_cov_tbl` <- function(x, i, j, drop = FALSE) {
  ref_alt_cov_tbl_reconstruct(NextMethod())
}

#' @export
`names<-.ref_alt_cov_tbl` <- function(x, value) {
  ref_alt_cov_tbl_reconstruct(NextMethod())
}

#' @export
`$<-.ref_alt_cov_tbl` <- function(x, name, value) {
  ref_alt_cov_tbl_reconstruct(NextMethod())
}
