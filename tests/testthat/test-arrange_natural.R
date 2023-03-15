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
    dplyr::mutate(
      sample = forcats::as_factor(sample),
      gene = forcats::as_factor(gene)
    )

  expect_identical(arrange_natural(df, sample, gene), rf)
})

test_that("arranges factors", {
  fdf <- df %>%
    dplyr::mutate(
      dplyr::across(.cols = dplyr::everything(), .fns = ~ forcats::as_factor(.))
    )

  rf <- tibble::tribble(
    ~sample, ~gene,
    "D10-5", "mdr1",
    "D10-5", "atp6",
    "D10-15", "atp6",
    "D10-55", "atp6",
  ) %>%
    dplyr::mutate(
      sample = forcats::as_factor(sample),
      gene = forcats::fct_relevel(forcats::as_factor(gene), "atp6")
    )

  expect_identical(arrange_natural(fdf, sample), rf)
})

test_that("arranges numerics", {
  ndf <- tibble::tribble(
    ~sample, ~pos,
    "D10-55", 55,
    "D10-5", 10,
    "D10-5", 5,
    "D10-15", 15,
  )

  rf <- tibble::tribble(
    ~sample, ~pos,
    "D10-5", 5,
    "D10-5", 10,
    "D10-15", 15,
    "D10-55", 55,
  ) %>%
    dplyr::mutate(pos = forcats::as_factor(pos))

  expect_identical(arrange_natural(ndf, pos), rf)
})

test_that("warning if packages not intalled", {
  # Trick R to think packages not installed
  mockery::stub(arrange_natural, "rlang::is_installed", FALSE)
  expect_snapshot(arrange_natural(df, gene))
})
