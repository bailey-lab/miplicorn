df <- tibble::tribble(
  ~sample, ~gene,
  "D10-55", "atp6",
  "D10-5", "mdr1",
  "D10-5", "atp6",
  "D10-15", "atp6",
)

test_that("empty sort() returns input", {
  expect_identical(sort(df), df)
})

test_that("sorts alphanumerically", {
  rf <- tibble::tribble(
    ~sample, ~gene,
    "D10-5", "mdr1",
    "D10-5", "atp6",
    "D10-15", "atp6",
    "D10-55", "atp6",
  )

  expect_identical(sort(df, sample), rf)
})

test_that("sorts on multiple variables", {
  rf <- tibble::tribble(
    ~sample, ~gene,
    "D10-5", "atp6",
    "D10-5", "mdr1",
    "D10-15", "atp6",
    "D10-55", "atp6",
  )

  expect_identical(sort(df, sample, gene), rf)
})
