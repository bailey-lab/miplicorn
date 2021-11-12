# Test utility functions -------------------------------------------------------
test_that("detect empty file", {
  expect_true(empty_file("empty-file"))
})

test_that("detects named inputs", {
  expect_error(read_tbl_helper("small.csv", gene = "mdr1"))
  expect_error(read_tbl_helper("small.csv", gene == "mdr1", chrom = "13"))

  expect_snapshot_error(read_tbl_helper("small.csv", gene = "mdr1"))
  expect_snapshot_error(read_tbl_helper("small.csv", gene == "g1", chrom = "8"))
})

# Test read_tbl_helper() -------------------------------------------------------
test_that("handles named logical vectors", {
  skip_on_cran()

  filters_tb <- tibble::tibble(keep = c(TRUE, FALSE))
  filters_df <- data.frame(keep = c(TRUE, FALSE))

  expect_identical(
    read_tbl_helper("small.csv", !!filters_df),
    read_tbl_helper("small.csv", !!filters_tb)
  )

  expect_identical(
    read_tbl_helper("small.csv", !!filters_df),
    read_tbl_helper("small.csv", !!unname(filters_df))
  )
})

test_that("returns an empty tibble on an empty file", {
  expect_equal(read_tbl_helper("empty-file"), tibble::tibble())
})

test_that("fails if filter variable is non-existent", {
  expect_error(read_tbl_helper("small.csv", var == 5))
  expect_snapshot_error(read_tbl_helper("small.csv", var == 5))
})

test_that("selection works properly", {
  res <- tibble::tribble(
    ~sample, ~gene_id, ~gene, ~mutation, ~func, ~aa_chng, ~target, ~value,
    "s1", "Site1", "atp6", "atp6-A623E", "missense", "A623E", "Yes", 0,
    "s2", "Site1", "atp6", "atp6-A623E", "missense", "A623E", "Yes", 0,
    "s1", "Site2", "mdr1", "mdr1-N86Y", "sense", "N86Y", "Yes", 13,
    "s2", "Site2", "mdr1", "mdr1-N86Y", "sense", "N86Y", "Yes", 0
  )

  expect_equal(
    read_tbl_helper("small.csv", gene == "mdr1"),
    dplyr::filter(res, gene == "mdr1")
  )

  expect_equal(
    read_tbl_helper("small.csv", gene == "mdr1" | func == "missense"),
    res
  )

  expect_equal(
    read_tbl_helper("small.csv", gene == "mdr1" & func == "missense"),
    dplyr::slice_sample(res, n = 0)
  )

  expect_equal(
    read_tbl_helper("small.csv", gene == "mdr1", func == "missense"),
    dplyr::slice_sample(res, n = 0)
  )
})

# Test read_tbl_ref_alt_cov()
test_that("chrom and gene are deprecated", {
  expect_snapshot(read_tbl_ref_alt_cov(
    .tbl_ref = "small.csv",
    .tbl_alt = "small.csv",
    .tbl_cov = "small.csv",
    chrom = "13"
  ))

  expect_snapshot(read_tbl_ref_alt_cov(
    .tbl_ref = "small.csv",
    .tbl_alt = "small.csv",
    .tbl_cov = "small.csv",
    gene = "atp"
  ))

  expect_snapshot(
    error = TRUE,
    read_tbl_ref_alt_cov(
      .tbl_ref = "small.csv",
      .tbl_alt = "small.csv",
      .tbl_cov = "small.csv",
      chrom = "13",
      gene = "atp"
    )
  )
})

test_that("read returns error if a file is empty", {
  expect_error(read_tbl_ref_alt_cov("empty-file", "empty-file", "empty-file"))
  expect_error(read_tbl_ref_alt_cov("small.csv", "empty-file", "empty-file"))
  expect_error(read_tbl_ref_alt_cov("small.csv", "small.csv", "empty-file"))
  expect_snapshot_error(read_tbl_ref_alt_cov(
    .tbl_ref = "empty-file",
    .tbl_alt = "empty-file",
    .tbl_cov = "empty-file"
  ))
  expect_snapshot_error(read_tbl_ref_alt_cov(
    .tbl_ref = "small.csv",
    .tbl_alt = "empty-file",
    .tbl_cov = "empty-file"
  ))
})
