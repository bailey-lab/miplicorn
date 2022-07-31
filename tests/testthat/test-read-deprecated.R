test_that("read() is deprecated", {
  expect_snapshot_warning(read("small.csv", "small.csv", "small.csv"))
})

test_that("read_file() is deprecated", {
  expect_snapshot(read_file("small.csv", .name = "ref_umi_count"))
})

res <- tibble::tribble(
  ~sample, ~gene_id, ~gene, ~mutation, ~func, ~aa_chng, ~target, ~value,
  "S1", "Site1", "atp6", "atp6-A623E", "missense", "A623E", "Yes", 0,
  "S2", "Site1", "atp6", "atp6-A623E", "missense", "A623E", "Yes", 0,
  "S1", "Site2", "mdr1", "mdr1-N86Y", "sense", "N86Y", "Yes", 13,
  "S2", "Site2", "mdr1", "mdr1-N86Y", "sense", "N86Y", "Yes", 0
)

test_that("deprecated read_file() detects reference tables", {
  expect_snapshot(read_file("small.csv", .name = "ref_umi_count"))
  expect_equal(
    suppressWarnings(suppressMessages(read_file(
      .file = "small.csv",
      .name = "ref_umi_count"
    ))),
    dplyr::rename(res, ref_umi_count = value),
    ignore_attr = "class"
  )
})

test_that("deprecated read_file() detects alternate tables", {
  expect_snapshot(read_file("small.csv", .name = "alt_umi_count"))
  expect_equal(
    suppressWarnings(suppressMessages(read_file(
      .file = "small.csv",
      .name = "alt_umi_count"
    ))),
    dplyr::rename(res, alt_umi_count = value),
    ignore_attr = "class"
  )
})

test_that("deprecated read_file() detects coverage tables", {
  expect_snapshot(read_file("small.csv", .name = "coverage"))
  expect_equal(
    suppressWarnings(suppressMessages(read_file(
      .file = "small.csv",
      .name = "coverage"
    ))),
    dplyr::rename(res, coverage = value),
    ignore_attr = "class"
  )
})

test_that("deprecated read_file() fails if can't detect input", {
  expect_snapshot(
    error = TRUE,
    read_file("small.csv")
  )
})
