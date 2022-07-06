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

#' @importFrom tibble tbl_sum
#' @export
tbl_sum.ref_tbl <- function(x) {
  c("A reference table" = pillar::dim_desc(x))
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

#' @export
tbl_sum.alt_tbl <- function(x) {
  c("An alternate table" = pillar::dim_desc(x))
}

# Coverage table ---------------------------------------------------------------
new_cov_tbl <- function(x) {
  tibble::new_tibble(x, class = "cov_tbl")
}

#' @export
`[.cov_tbl` <- function(x, i, j, drop = FALSE) {
  cov_tbl_reconstruct(NextMethod())
}

#' @export
`names<-.cov_tbl` <- function(x, value) {
  cov_tbl_reconstruct(NextMethod())
}

#' @export
`$<-.cov_tbl` <- function(x, name, value) {
  cov_tbl_reconstruct(NextMethod())
}

#' @export
tbl_sum.cov_tbl <- function(x) {
  c("A coverage table" = pillar::dim_desc(x))
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

#' @export
tbl_sum.geno_tbl <- function(x) {
  c("A genotype table" = pillar::dim_desc(x))
}

# Haplotype table --------------------------------------------------------------
new_hap_tbl <- function(x) {
  tibble::new_tibble(x, class = "hap_tbl")
}

#' @export
`[.hap_tbl` <- function(x, i, j, drop = FALSE) {
  hap_tbl_reconstruct(NextMethod())
}

#' @export
`names<-.hap_tbl` <- function(x, value) {
  hap_tbl_reconstruct(NextMethod())
}

#' @export
`$<-.hap_tbl` <- function(x, name, value) {
  hap_tbl_reconstruct(NextMethod())
}

#' @export
tbl_sum.hap_tbl <- function(x) {
  c("A haplotype table" = pillar::dim_desc(x))
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

#' @export
tbl_sum.ref_alt_cov_tbl <- function(x) {
  c("A ref alt cov table" = pillar::dim_desc(x))
}
