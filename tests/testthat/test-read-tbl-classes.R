# Example data -----------------------------------------------------------------
ref <- new_ref_tbl(tibble::tribble(
  ~sample, ~ref_umi_count,
  "sample1", 11,
  "sample2", 100,
))

alt <- new_alt_tbl(tibble::tribble(
  ~sample, ~alt_umi_count,
  "sample1", 11,
  "sample2", 100,
))

cov <- new_cov_tbl(tibble::tribble(
  ~sample, ~coverage,
  "sample1", 11,
  "sample2", 100,
))

geno <- new_geno_tbl(tibble::tribble(
  ~sample, ~targeted, ~genotype,
  "sample1", "No", 1,
  "sample2", "Yes", 0,
))

hap <- new_hap_tbl(tibble::tribble(
  ~sample, ~haplotype_id, ~haplotype_sequence,
  "sample1", "mip1", "CATG",
  "sample2", "mip2", "TTGG",
))

ref_alt_cov <- new_ref_alt_cov_tbl(tibble::tribble(
  ~sample, ~ref_umi_count, ~alt_umi_count, ~coverage,
  "sample1", 5, 1, 6,
  "sample2", 0, 5, 6,
))

# Tests ------------------------------------------------------------------------
test_that("subclass correctely assigned", {
  df <- tibble::tibble(a = 1, b = 2)

  # Reference table
  expect_s3_class(
    new_ref_tbl(df),
    c("ref_tbl", "tbl_df", "tbl", "data.frame"),
    exact = TRUE
  )

  # Alternate table
  expect_s3_class(
    new_alt_tbl(df),
    c("alt_tbl", "tbl_df", "tbl", "data.frame"),
    exact = TRUE
  )

  # Coverage table
  expect_s3_class(
    new_cov_tbl(df),
    c("cov_tbl", "tbl_df", "tbl", "data.frame"),
    exact = TRUE
  )

  # Genotype table
  expect_s3_class(
    new_geno_tbl(df),
    c("geno_tbl", "tbl_df", "tbl", "data.frame"),
    exact = TRUE
  )

  # Haplotype table
  expect_s3_class(
    new_hap_tbl(df),
    c("hap_tbl", "tbl_df", "tbl", "data.frame"),
    exact = TRUE
  )

  # Ref, alt, coverage table
  expect_s3_class(
    new_ref_alt_cov_tbl(df),
    c("ref_alt_cov_tbl", "tbl_df", "tbl", "data.frame"),
    exact = TRUE
  )
})

test_that("genotype column only contains NA, -1, 0, 1, 2", {
  geno$genotype <- c(NA, -1)
  expect_s3_class(geno, "geno_tbl")

  geno$genotype <- c(10, 0)
  expect_s3_class(geno, c("tbl_df", "tbl", "data.frame"), exact = TRUE)
})

test_that("can subset object", {
  # Reference table
  expect_s3_class(ref[1, ], "ref_tbl")
  expect_s3_class(ref[, 1], c("tbl_df", "tbl", "data.frame"), exact = TRUE)

  # Alternate table
  expect_s3_class(alt[1, ], "alt_tbl")
  expect_s3_class(alt[, 1], c("tbl_df", "tbl", "data.frame"), exact = TRUE)

  # Coverage table
  expect_s3_class(cov[1, ], "cov_tbl")
  expect_s3_class(cov[, 1], c("tbl_df", "tbl", "data.frame"), exact = TRUE)

  # Genotype table
  expect_s3_class(geno[1, ], "geno_tbl")
  expect_s3_class(geno[, 1], c("tbl_df", "tbl", "data.frame"), exact = TRUE)

  # Haplotype table
  expect_s3_class(hap[1, ], "hap_tbl")
  expect_s3_class(hap[, 1], c("tbl_df", "tbl", "data.frame"), exact = TRUE)

  # Ref, alt, coverage table
  expect_s3_class(ref_alt_cov[1, ], "ref_alt_cov_tbl")
  expect_s3_class(
    ref_alt_cov[, 1],
    c("tbl_df", "tbl", "data.frame"),
    exact = TRUE
  )
})

test_that("can rename columns", {
  # Reference table
  names(ref) <- c("a", "b")
  expect_s3_class(ref, c("tbl_df", "tbl", "data.frame"), exact = TRUE)

  # Alternate table
  names(alt) <- c("a", "b")
  expect_s3_class(alt, c("tbl_df", "tbl", "data.frame"), exact = TRUE)

  # Coverage table
  names(cov) <- c("a", "b")
  expect_s3_class(cov, c("tbl_df", "tbl", "data.frame"), exact = TRUE)

  # Genotype table
  names(geno) <- c("sample", "t", "genotype")
  expect_s3_class(geno, "geno_tbl")

  names(geno) <- c("a", "b", "c")
  expect_s3_class(geno, c("tbl_df", "tbl", "data.frame"), exact = TRUE)

  # Haplotype table
  names(hap) <- c("a", "b", "c")
  expect_s3_class(hap, c("tbl_df", "tbl", "data.frame"), exact = TRUE)

  # Ref, alt, coverage table
  names(ref_alt_cov) <- c("a", "b", "c", "d")
  expect_s3_class(ref_alt_cov, c("tbl_df", "tbl", "data.frame"), exact = TRUE)
})

test_that("can reassign object", {
  # Reference table
  ref$sample <- NULL
  expect_s3_class(ref, c("tbl_df", "tbl", "data.frame"), exact = TRUE)

  # Alternate table
  alt$sample <- NULL
  expect_s3_class(alt, c("tbl_df", "tbl", "data.frame"), exact = TRUE)

  # Coverage table
  cov$sample <- NULL
  expect_s3_class(cov, c("tbl_df", "tbl", "data.frame"), exact = TRUE)

  # Genotype table
  geno$targeted <- c("No", "No")
  expect_s3_class(geno, "geno_tbl")

  geno$genotype <- NULL
  expect_s3_class(geno, c("tbl_df", "tbl", "data.frame"), exact = TRUE)

  # Haplotype table
  hap$sample <- NULL
  expect_s3_class(hap, c("tbl_df", "tbl", "data.frame"), exact = TRUE)

  # Ref, alt, coverage table
  ref_alt_cov$coverage <- NULL
  expect_s3_class(ref_alt_cov, c("tbl_df", "tbl", "data.frame"), exact = TRUE)
})

test_that("class is dplyr compatible", {
  # Reference table
  expect_s3_class(
    dplyr::select(ref, 1),
    c("tbl_df", "tbl", "data.frame"),
    exact = TRUE
  )
  expect_s3_class(dplyr::filter(ref, ref_umi_count > 50), "ref_tbl")

  # Alternate table
  expect_s3_class(
    dplyr::select(alt, 1),
    c("tbl_df", "tbl", "data.frame"),
    exact = TRUE
  )
  expect_s3_class(dplyr::filter(alt, alt_umi_count > 50), "alt_tbl")

  # Coverage table
  expect_s3_class(
    dplyr::select(cov, 1),
    c("tbl_df", "tbl", "data.frame"),
    exact = TRUE
  )
  expect_s3_class(dplyr::filter(cov, coverage > 50), "cov_tbl")

  # Genotype table
  expect_s3_class(
    dplyr::select(geno, 1, 2),
    c("tbl_df", "tbl", "data.frame"),
    exact = TRUE
  )
  expect_s3_class(dplyr::select(geno, 1, 3), "geno_tbl")
  expect_s3_class(dplyr::filter(geno, targeted == "Yes"), "geno_tbl")

  # Haplotype table
  expect_s3_class(
    dplyr::select(hap, 1),
    c("tbl_df", "tbl", "data.frame"),
    exact = TRUE
  )
  expect_s3_class(dplyr::filter(hap, haplotype_id == "mip1"), "hap_tbl")

  # Ref, alt, coverage table
  expect_s3_class(
    dplyr::select(ref_alt_cov, 1, 2),
    c("tbl_df", "tbl", "data.frame"),
    exact = TRUE
  )
  expect_s3_class(dplyr::filter(ref_alt_cov, coverage > 5), "ref_alt_cov_tbl")
})

# Test tbl_sum methods
test_that("tbl_sum methods work", {
  expect_snapshot(print(ref))
  expect_snapshot(print(alt))
  expect_snapshot(print(cov))
  expect_snapshot(print(geno))
  expect_snapshot(print(hap))
  expect_snapshot(print(ref_alt_cov))
})
