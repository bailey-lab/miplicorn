test_that("detect empty file", {
  expect_true(empty_file("empty-file"))
})

test_that("read_file returns an empty tibble on an empty file", {
  expect_equal(read_file("empty-file"), tibble::tibble())
})

test_that("read returns an empty tibble on an empty file", {
  expect_equal(read("empty-file", "empty-file", "empty-file"), tibble::tibble())
})

test_that("chrom and gene are deprecated", {
  expect_warning(suppressMessages(
    read("small.csv", "small.csv", "small.csv", chrom = "13"))
  )
  expect_warning(suppressMessages(
    read("small.csv", "small.csv", "small.csv", gene = "atp"))
  )

  expect_error(suppressMessages(suppressWarnings(
    read("small.csv", "small.csv", "small.csv", chrom = "13", gene = "atp")))
  )
  expect_snapshot(
    error = TRUE,
    suppressMessages(
      read("small.csv", "small.csv", "small.csv", chrom = "13", gene = "10")
    )
  )
})

test_that("named filter inputs returns error", {
  expect_error(read_file("small.csv", gene = "mdr1", .name = value))

  expect_snapshot(
    error = TRUE,
    read_file("small.csv", gene = "mdr1", .name = value)
  )
})

test_that("handles named logical vectors", {
  skip_on_cran()

  filters_tb <- tibble::tibble(keep = c(TRUE, FALSE))
  filters_df <- data.frame(keep = c(TRUE, FALSE))

  expect_identical(
    read_file("small.csv", !!filters_df),
    read_file("small.csv", !!filters_tb)
  )

  expect_identical(
    read_file("small.csv", !!filters_df),
    read_file("small.csv", !!unname(filters_df))
  )
})
