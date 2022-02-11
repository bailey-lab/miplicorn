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

test_that("filter variable must exist", {
  df <- tibble::tribble(
    ~sample, ~gene,
    "S1", "atp6",
    "S2", "mdr1",
    "S3", "atp6",
    "S4", "atp6"
  )

  expect_error(filter_tbl(df, var == 5))
  expect_snapshot_error(filter_tbl(df, var == 5))
})

# Test read_tbl_helper() -------------------------------------------------------
test_that("returns an empty tibble on an empty file", {
  expect_equal(read_tbl_helper("empty-file"), tibble::tibble())
})

test_that("selection works properly", {
  res <- tibble::tribble(
    ~sample, ~gene_id, ~gene, ~mutation, ~func, ~aa_chng, ~target, ~value,
    "S1", "Site1", "atp6", "atp6-A623E", "missense", "A623E", "Yes", 0,
    "S2", "Site1", "atp6", "atp6-A623E", "missense", "A623E", "Yes", 0,
    "S1", "Site2", "mdr1", "mdr1-N86Y", "sense", "N86Y", "Yes", 13,
    "S2", "Site2", "mdr1", "mdr1-N86Y", "sense", "N86Y", "Yes", 0
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
    dplyr::filter(res, gene == "mdr1" & func == "missense")
  )

  expect_equal(
    read_tbl_helper("small.csv", gene == "mdr1", func == "missense"),
    dplyr::filter(res, gene == "mdr1", func == "missense")
  )
})

test_that("sample IDs remain unchanged (#20)", {
  res <- tibble::tribble(
    ~sample, ~gene_id, ~gene, ~mutation, ~func, ~aa_chng, ~target, ~value,
    "S1-55", "Site1", "atp6", "atp6-A623E", "missense", "A623E", "Yes", 0,
    "s_2-73", "Site1", "atp6", "atp6-A623E", "missense", "A623E", "Yes", 0,
    "S1-55", "Site2", "mdr1", "mdr1-N86Y", "sense", "N86Y", "Yes", 13,
    "s_2-73", "Site2", "mdr1", "mdr1-N86Y", "sense", "N86Y", "Yes", 0
  )

  expect_equal(read_tbl_helper("preserve-sample-id.csv"), res)
})

# Test read_tbl_haplotypes() ---------------------------------------------------
test_that("returns an empty tibble on an empty file", {
  expect_equal(read_tbl_haplotype("empty-file"), tibble::tibble())
})

haplotype_res <- tibble::tribble(
  ~sample, ~barcode_count, ~chrom, ~gene, ~pos,
  "sample-1", 353L, "chr6", "DHODH", 130330L,
  "sample-2", 275L, "chr3", "pfabcI3", 823117L
)

test_that("cleans column names", {
  expect_equal(read_tbl_haplotype("haplotype_test.csv"), haplotype_res)
})

test_that("can select columns", {
  expect_equal(
    read_tbl_haplotype("haplotype_test.csv", .col_select = -POS),
    dplyr::select(haplotype_res, -pos)
  )
})

test_that("can filter rows", {
  expect_equal(
    read_tbl_haplotype("haplotype_test.csv", gene == "DHODH"),
    dplyr::filter(haplotype_res, gene == "DHODH")
  )

  expect_equal(
    read_tbl_haplotype("haplotype_test.csv", gene == "DHODH" | chrom == "chr3"),
    haplotype_res
  )

  expect_equal(
    read_tbl_haplotype("haplotype_test.csv", gene == "DHODH" & chrom == "chr3"),
    dplyr::filter(haplotype_res, gene == "DHODH" & chrom == "chr3")
  )

  expect_equal(
    read_tbl_haplotype("haplotype_test.csv", gene == "DHODH", chrom == "chr3"),
    dplyr::filter(haplotype_res, gene == "DHODH", chrom == "chr3")
  )
})

# Test read_tbl_ref_alt_cov() --------------------------------------------------
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
