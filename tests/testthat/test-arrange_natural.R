df <- tibble::tribble(
  ~sample, ~gene,
  "D10-55", "atp6",
  "D10-5", "mdr1",
  "D10-5", "atp6",
  "D10-15", "atp6",
)

test_that("empty arrange_natural() returns input", {
  expect_identical(arrange_natural(df), df)
})

test_that("applies natural sorting", {
  rf <- tibble::tribble(
    ~sample, ~gene,
    "D10-5", "mdr1",
    "D10-5", "atp6",
    "D10-15", "atp6",
    "D10-55", "atp6",
  ) %>%
    dplyr::mutate(sample = forcats::as_factor(sample))

  expect_identical(arrange_natural(df, sample), rf)
})

test_that("sorts on multiple variables", {
  rf <- tibble::tribble(
    ~sample, ~gene,
    "D10-5", "atp6",
    "D10-5", "mdr1",
    "D10-15", "atp6",
    "D10-55", "atp6",
  ) %>%
    dplyr::mutate(sample = forcats::as_factor(sample),
                  gene = forcats::as_factor(gene))

  expect_identical(arrange_natural(df, sample, gene), rf)
})
