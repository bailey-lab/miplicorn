data <- read_tbl_ref_alt_cov(
  miplicorn_example("reference_AA_table.csv"),
  miplicorn_example("alternate_AA_table.csv"),
  miplicorn_example("coverage_AA_table.csv"),
  gene == "atp6" | gene == "crt"
)

plot <- new_mut_prev(tibble::tribble(
  ~mutation_name, ~n_total, ~n_mutant, ~prevalence,
  "atp6-Ala623Glu", 36L, NA, NA,
  "atp6-Glu431Lys", 39L, NA, NA,
  "atp6-Gly639Asp", 26L, 19L, 0.730769230769231,
  "atp6-Ser466Asn", 15L, 9L, 0.6,
  "atp6-Ser769Asn", 17L, NA, NA,
  "crt-Ala220Ser", 11L, 4L, 0.363636363636364,
  "crt-Asn326Asp", 21L, 8L, 0.380952380952381,
  "crt-Asn326Ser", 26L, NA, NA,
  "crt-Asn75Glu", 29L, 24L, 0.827586206896552,
  "crt-Cys72Ser", 31L, 23L, 0.741935483870968,
  "crt-His97Leu", 47L, NA, NA,
  "crt-His97Tyr", 47L, NA, NA,
  "crt-Ile356Leu", 22L, 15L, 0.681818181818182,
  "crt-Ile356Thr", 41L, 18L, 0.439024390243902,
  "crt-Lys76Thr", 29L, 25L, 0.862068965517241,
  "crt-Met74Ile", 29L, 24L, 0.827586206896552
))

# mut_prev class Test Cases ----------------------------------------------------
simple <- new_mut_prev(tibble::tribble(
  ~mutation_name, ~n_total, ~n_mutant, ~prevalence,
  "atp6-Ala623Glu", 30L, 15, .50,
  "atp6-Glu431Lys", 40L, 20, .50
))

test_that("subclass correctely assigned", {
  df <- tibble::tibble(a = 1, b = 2)
  expect_s3_class(
    new_mut_prev(df),
    c("mut_prev", "tbl_df", "tbl", "data.frame"),
    exact = TRUE
  )
})

test_that("can subset object", {
  expect_s3_class(simple[1, ], "mut_prev")
  expect_s3_class(simple[, 1], c("tbl_df", "tbl", "data.frame"), exact = TRUE)
})

test_that("can rename object", {
  names(simple) <- c("mutation_name", "a", "b", "prevalence")
  expect_s3_class(simple, "mut_prev")

  names(simple) <- c("a", "b", "c", "d")
  expect_s3_class(simple, c("tbl_df", "tbl", "data.frame"), exact = TRUE)
})

test_that("can reassign object", {
  simple$n_total <- c(50, 60)
  expect_s3_class(simple, "mut_prev")

  simple$prevalence <- NULL
  expect_s3_class(simple, c("tbl_df", "tbl", "data.frame"), exact = TRUE)
})

test_that("class is dplyr compatible", {
  expect_s3_class(
    dplyr::select(simple, 1, 2),
    c("tbl_df", "tbl", "data.frame"),
    exact = TRUE
  )
  expect_s3_class(dplyr::select(simple, 1, 4), "mut_prev")
  expect_s3_class(dplyr::filter(simple, prevalence > 0.4), "mut_prev")
})

# mutation_prevalence() Test Cases ---------------------------------------------
test_that("error if lack ref, alt, coverage columns", {
  expect_snapshot_error(mutation_prevalence(data[, 1:5], 3))
})

test_that("error if lack mutation_name column", {
  expect_snapshot_error(mutation_prevalence(data[, 6:10], 3))
})

test_that("output has unique mutation names", {
  out <- mutation_prevalence(data, 3)
  expect_setequal(out$mutation_name, unique(out$mutation_name))
})

test_that("result inherits new class", {
  expect_s3_class(mutation_prevalence(data, 3), "mut_prev")
})

test_that("results computed correctly", {
  expect_equal(mutation_prevalence(data, 5), plot)
})

# plot_mutation_prevalence() Test Cases ----------------------------------------
test_that("data must have mut_prev class", {
  df <- tibble::tibble(a = 1, b = 2)
  expect_snapshot_error(plot_mutation_prevalence(df))
})

test_that("creates a nice plot", {
  vdiffr::expect_doppelganger(
    "default plot is informative",
    plot_mutation_prevalence(plot)
  )
})

test_that("plot and autoplot methods work", {
  vdiffr::expect_doppelganger("autoplot method works", autoplot(plot))
  vdiffr::expect_doppelganger("plot method works", plot(plot))
})
