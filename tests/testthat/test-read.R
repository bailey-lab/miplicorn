test_that("detect empty file", {
  expect_true(empty_file("empty-file"))
})

test_that("read_file returns an empty tibble on an empty file", {
  expect_equal(read_file("empty-file"), tibble::tibble())
})

test_that("read returns an empty tibble on an empty file", {
  expect_equal(read("empty-file", "empty-file", "empty-file"), tibble::tibble())
})
