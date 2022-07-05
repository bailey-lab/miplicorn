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
