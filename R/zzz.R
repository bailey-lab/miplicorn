# nocov start

.onLoad <- function(libname, pkgname) {
  if (
    rlang::is_installed("dplyr") &&
      utils::packageVersion("dplyr") >= "1.0.0"
  ) {
    vctrs::s3_register(
      "dplyr::dplyr_reconstruct",
      "mut_prev",
      method = mut_prev_reconstruct
    )
  }
  invisible()
}

# nocov end
