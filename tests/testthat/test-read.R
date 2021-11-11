test_that("detect empty file", {
  expect_true(empty_file("empty-file"))
})

test_that("read_file returns an empty tibble on an empty file", {
  expect_equal(read_file("empty-file"), tibble::tibble())
})

test_that("read returns error if a file is empty", {
  expect_error(read("empty-file", "empty-file", "empty-file"))
  expect_error(read("small.csv", "empty-file", "empty-file"))
  expect_error(read("small.csv", "small.csv", "empty-file"))
  expect_snapshot(error = TRUE, read("empty-file", "empty-file", "empty-file"))
  expect_snapshot(error = TRUE, read("small.csv", "empty-file", "empty-file"))
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
