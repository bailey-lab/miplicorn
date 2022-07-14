# Test utility functions -------------------------------------------------------
test_that("detect empty file", {
  expect_true(empty_file("empty-file"))
})

test_that("detects named inputs", {
  expect_error(read_tbl_reference("small.csv", gene = "mdr1"))
  expect_error(read_tbl_alternate("small.csv", gene == "mdr1", chrom = "13"))

  expect_snapshot_error(read_tbl_coverage("small.csv", gene = "mdr1"))
  expect_snapshot_error(read_tbl_haplotype("small.csv", gene = "g1"))
  expect_snapshot_error(
    read_tbl_ref_alt_cov("small.csv", "small.csv", "small.csv", var = "g1")
  )
})

test_that("filter variable must exist", {
  expect_error(read_tbl_reference("small.csv", wrong == 5))
  expect_snapshot_error(
    read_tbl_ref_alt_cov("small.csv", "small.csv", "small.csv", var == 5)
  )
})

test_that("error if filter variable doesn't exist is pluralized properly", {
  one_object <- tibble::tibble(sample = "S1", gene = "atp6")

  expect_error(filter_tbl(one_object, var == 5))
  expect_snapshot_error(filter_tbl(one_object, var == 5))

  multiple_objects <- tibble::tibble(sample = "S1", gene = "atp", target = "No")

  expect_error(filter_tbl(multiple_objects, var == 5))
  expect_snapshot_error(filter_tbl(multiple_objects, var == 5))
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
  "sample-1", 353, "chr6", "DHODH", 130330,
  "sample-2", 275, "chr3", "pfabcI3", 823117
)

test_that("cleans column names", {
  expect_equal(
    read_tbl_haplotype("haplotype_test.csv"),
    haplotype_res,
    ignore_attr = c("class", "spec", "problems")
  )
})

test_that("can select columns", {
  expect_equal(
    read_tbl_haplotype("haplotype_test.csv", .col_select = -POS),
    dplyr::select(haplotype_res, -pos),
    ignore_attr = c("class", "spec", "problems")
  )
})

test_that("can filter rows", {
  expect_equal(
    read_tbl_haplotype("haplotype_test.csv", gene == "DHODH"),
    dplyr::filter(haplotype_res, gene == "DHODH"),
    ignore_attr = c("class", "spec", "problems")
  )

  expect_equal(
    read_tbl_haplotype("haplotype_test.csv", gene == "DHODH" | chrom == "chr3"),
    haplotype_res,
    ignore_attr = c("class", "spec", "problems")
  )

  expect_equal(
    read_tbl_haplotype("haplotype_test.csv", gene == "DHODH" & chrom == "chr3"),
    dplyr::filter(haplotype_res, gene == "DHODH" & chrom == "chr3"),
    ignore_attr = c("class", "spec", "problems")
  )

  expect_equal(
    read_tbl_haplotype("haplotype_test.csv", gene == "DHODH", chrom == "chr3"),
    dplyr::filter(haplotype_res, gene == "DHODH", chrom == "chr3"),
    ignore_attr = c("class", "spec", "problems")
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

test_that("read_tbl_ref_alt_cov returns error if a file is empty", {
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
