test_that("filter numeric columns", {
  data <- tibble::tribble(
    ~alt_umi_count, ~coverage, ~ref_umi_count,
    20, 30, 10,
    40, 15, 20,
  )

  expect_equal(
    suppressWarnings(filter_alt_umi_count(data, 30)),
    tibble::tribble(
      ~alt_umi_count, ~coverage, ~ref_umi_count,
      40, 15, 20,
    )
  )
  expect_equal(
    suppressWarnings(filter_coverage(data, 20)),
    tibble::tribble(
      ~alt_umi_count, ~coverage, ~ref_umi_count,
      20, 30, 10,
    )
  )
  expect_equal(
    suppressWarnings(filter_ref_umi_count(data, 40)),
    tibble::tibble(
      alt_umi_count = numeric(0),
      coverage = numeric(0),
      ref_umi_count = numeric(0)
    )
  )
})

test_that("filter character columns", {
  data <- tibble::tribble(
    ~aa_change, ~gene, ~mutation_name, ~targeted,
    "A6T", "atp", "atp-A6T", "Yes",
    "C70G", "crt", "crt-C70G", "No"
  )

  expect_equal(
    suppressWarnings(filter_aa_change(data, "A6T")),
    tibble::tribble(
      ~aa_change, ~gene, ~mutation_name, ~targeted,
      "A6T", "atp", "atp-A6T", "Yes",
    )
  )
  expect_equal(
    suppressWarnings(filter_gene(data, "crt")),
    tibble::tribble(
      ~aa_change, ~gene, ~mutation_name, ~targeted,
      "C70G", "crt", "crt-C70G", "No"
    )
  )
  expect_equal(
    suppressWarnings(filter_mutation_name(data, "atp-A6T")),
    tibble::tribble(
      ~aa_change, ~gene, ~mutation_name, ~targeted,
      "A6T", "atp", "atp-A6T", "Yes",
    )
  )
  expect_equal(
    suppressWarnings(filter_targeted(data, "No")),
    tibble::tribble(
      ~aa_change, ~gene, ~mutation_name, ~targeted,
      "C70G", "crt", "crt-C70G", "No"
    )
  )
})

test_that("error if column does not exist", {
  df <- tibble::tibble(a = 2, b = 3)
  expect_snapshot_error(filter_gene(df, "atp6"))
})

test_that("filter_*() functions advise user to use dplyr::filter()", {
  withr::with_options(
    list(rlib_warning_verbosity = "verbose"),
    expect_snapshot(filter_coverage(tibble::tibble(coverage = c(2, 3)), 2))
  )
})
