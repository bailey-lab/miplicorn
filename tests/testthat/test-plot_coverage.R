data <- read_tbl_coverage(miplicorn_example("coverage_AA_table.csv"))

test_that("data must have mut_prev class", {
  df <- tibble::tibble(a = 1, b = 2)
  expect_error(plot_coverage(df, b))
  expect_snapshot_error(plot_coverage(df, b))
})

test_that("creates a nice plot", {
  vdiffr::expect_doppelganger(
    "default plot is informative",
    plot_coverage(data, gene)
  )
})

test_that("can add ggplot2 layers", {
  vdiffr::expect_doppelganger(
    "can add ggplot2 layers",
    plot_coverage(data, gene) +
      ggplot2::labs(x = "Gene", y = "Average Coverage")
  )
})
