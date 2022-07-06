genotype <- new_geno_tbl(tibble::tribble(
  ~sample, ~targeted, ~genotype,
  "sample1", "No", 1,
  "sample2", "Yes", 0,
))

ref_alt_cov <- new_ref_alt_cov_tbl(tibble::tribble(
  ~sample, ~ref_umi_count, ~alt_umi_count, ~coverage,
  "sample1", 5, 1, 6,
  "sample2", 0, 5, 6,
))

test_that("subclass correctely assigned", {
  df <- tibble::tibble(a = 1, b = 2)
  expect_s3_class(
    new_geno_tbl(df),
    c("geno_tbl", "tbl_df", "tbl", "data.frame"),
    exact = TRUE
  )
  expect_s3_class(
    new_ref_alt_cov_tbl(df),
    c("ref_alt_cov_tbl", "tbl_df", "tbl", "data.frame"),
    exact = TRUE
  )
})

test_that("genotype column only contains NA, -1, 0, 1, 2", {
  genotype$genotype <- c(NA, -1)
  expect_s3_class(genotype, "geno_tbl")

  genotype$genotype <- c(10, 0)
  expect_s3_class(genotype, c("tbl_df", "tbl", "data.frame"), exact = TRUE)
})

test_that("can subset object", {
  expect_s3_class(genotype[1, ], "geno_tbl")
  expect_s3_class(genotype[, 1], c("tbl_df", "tbl", "data.frame"), exact = TRUE)

  expect_s3_class(ref_alt_cov[1, ], "ref_alt_cov_tbl")
  expect_s3_class(
    ref_alt_cov[, 1],
    c("tbl_df", "tbl", "data.frame"),
    exact = TRUE
  )
})

test_that("can rename columns", {
  names(genotype) <- c("sample", "t", "genotype")
  expect_s3_class(genotype, "geno_tbl")

  names(genotype) <- c("a", "b", "c")
  expect_s3_class(genotype, c("tbl_df", "tbl", "data.frame"), exact = TRUE)

  names(ref_alt_cov) <- c("a", "b", "c", "d")
  expect_s3_class(ref_alt_cov, c("tbl_df", "tbl", "data.frame"), exact = TRUE)
})

test_that("can reassign object", {
  genotype$targeted <- c("No", "No")
  expect_s3_class(genotype, "geno_tbl")

  genotype$genotype <- NULL
  expect_s3_class(genotype, c("tbl_df", "tbl", "data.frame"), exact = TRUE)

  ref_alt_cov$coverage <- NULL
  expect_s3_class(ref_alt_cov, c("tbl_df", "tbl", "data.frame"), exact = TRUE)
})

test_that("class is dplyr compatible", {
  expect_s3_class(
    dplyr::select(genotype, 1, 2),
    c("tbl_df", "tbl", "data.frame"),
    exact = TRUE
  )
  expect_s3_class(dplyr::select(genotype, 1, 3), "geno_tbl")
  expect_s3_class(dplyr::filter(genotype, targeted == "Yes"), "geno_tbl")

  expect_s3_class(
    dplyr::select(ref_alt_cov, 1, 2),
    c("tbl_df", "tbl", "data.frame"),
    exact = TRUE
  )
  expect_s3_class(dplyr::filter(ref_alt_cov, coverage > 5), "ref_alt_cov_tbl")
})
