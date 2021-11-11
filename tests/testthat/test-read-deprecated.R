test_that("chrom and gene are deprecated", {
  expect_warning(suppressMessages(
    read("small.csv", "small.csv", "small.csv", chrom = "13")
  ))
  expect_warning(suppressMessages(
    read("small.csv", "small.csv", "small.csv", gene = "atp")
  ))

  expect_error(suppressMessages(suppressWarnings(
    read("small.csv", "small.csv", "small.csv", chrom = "13", gene = "atp")
  )))
  expect_snapshot(
    error = TRUE,
    suppressMessages(
      read("small.csv", "small.csv", "small.csv", chrom = "13", gene = "10")
    )
  )
})

test_that("read_file() is deprecated", {
  expect_warning(read_file("small.csv", .name = "ref_umi_count"))
  expect_snapshot(read_file("small.csv", .name = "ref_umi_count"))
})

test_that("deprecated read_file() detects correct inputs", {
  expect_message(read_file("small.csv", .name = "ref_umi_count"))
  expect_snapshot(read_file("small.csv", .name = "ref_umi_count"))

  expect_message(read_file("small.csv", .name = "alt_umi_count"))
  expect_snapshot(read_file("small.csv", .name = "alt_umi_count"))

  expect_message(read_file("small.csv", .name = "coverage"))
  expect_snapshot(read_file("small.csv", .name = "coverage"))
})

test_that("deprecated read_file() fails if can't detect input", {
  expect_error(read_file("small.csv"))
  expect_snapshot_error(read_file("small.csv"))
})
