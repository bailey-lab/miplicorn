data <- read_tbl_ref_alt_cov(
  miplicorn_example("reference_AA_table.csv"),
  miplicorn_example("alternate_AA_table.csv"),
  miplicorn_example("coverage_AA_table.csv"),
  gene == "atp6" | gene == "crt"
)

plot <- tibble::tribble(
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
)
class(plot) <- c("mutation_prev", class(plot))

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
  expect_s3_class(mutation_prevalence(data, 3), "mutation_prev")
})

test_that("results computed correctly", {
  expect_equal(mutation_prevalence(data, 5), plot)
})

# plot_mutation_prevalence() Test Cases ----------------------------------------
test_that("data must have mutation_prev class", {
  df <- tibble::tibble(a = 1, b = 2)
  expect_snapshot_error(plot_mutation_prevalence(df))
})

test_that("creates a nice plot", {
  vdiffr::expect_doppelganger(
    "default plot is informative",
    plot_mutation_prevalence(plot)
  )
})

test_that("can manipulate data and then plot", {
  vdiffr::expect_doppelganger(
    "can plot manipulated data",
    plot_mutation_prevalence(dplyr::filter(plot, prevalence > 0.7))
  )
})
