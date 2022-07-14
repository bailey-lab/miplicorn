# mut_freq class Test Cases ----------------------------------------------------
simple <- new_mut_freq(tibble::tribble(
  ~mutation_name, ~frequency,
  "atp6-Ala623Glu", 0.6,
  "atp6-Glu431Lys", 0.3
))

test_that("subclass correctely assigned", {
  df <- tibble::tibble(a = 1, b = 2)
  expect_s3_class(
    new_mut_freq(df),
    c("mut_freq", "tbl_df", "tbl", "data.frame"),
    exact = TRUE
  )
})

test_that("can subset object", {
  expect_s3_class(simple[1, ], "mut_freq")
  expect_s3_class(simple[, 1], c("tbl_df", "tbl", "data.frame"), exact = TRUE)
})

test_that("can't rename object", {
  names(simple) <- c("a", "b")
  expect_s3_class(simple, c("tbl_df", "tbl", "data.frame"), exact = TRUE)
})

test_that("can reassign object", {
  simple$frequency <- c(0.5, 0.6)
  expect_s3_class(simple, "mut_freq")

  simple$frequency <- NULL
  expect_s3_class(simple, c("tbl_df", "tbl", "data.frame"), exact = TRUE)
})

test_that("class is dplyr compatible", {
  expect_s3_class(
    dplyr::select(simple, 1),
    c("tbl_df", "tbl", "data.frame"),
    exact = TRUE
  )
  expect_s3_class(dplyr::select(simple, 1, 2), "mut_freq")
  expect_s3_class(dplyr::filter(simple, frequency > 0.4), "mut_freq")
})

# data setup  ------------------------------------------------------------------
data <- read_tbl_ref_alt_cov(
  miplicorn_example("reference_AA_table.csv"),
  miplicorn_example("alternate_AA_table.csv"),
  miplicorn_example("coverage_AA_table.csv"),
  gene == "atp6" | gene == "crt"
)

plot <- new_mut_freq(tibble::tribble(
  ~mutation_name, ~frequency,
  "atp6-Ala623Glu", 0,
  "atp6-Glu431Lys", 0,
  "atp6-Gly639Asp", 0.18321848398865,
  "atp6-Ser466Asn", 0.128688524590164,
  "atp6-Ser769Asn", 0,
  "crt-Ala220Ser", 0.127659574468085,
  "crt-Asn326Asp", 0.100929614873838,
  "crt-Asn326Ser", 0,
  "crt-Asn75Glu", 0.0533246272717379,
  "crt-Cys72Ser", 0.145971873977979,
  "crt-His97Leu", 0,
  "crt-His97Tyr", 0,
  "crt-Ile356Leu", 0.129661016949153,
  "crt-Ile356Thr", 0.0506868782567504,
  "crt-Lys76Thr", 0.200348242463815,
  "crt-Met74Ile", 0.0533246272717379
))

# mutation_frequency() Test Cases ---------------------------------------------
test_that("error if lack alt, coverage columns", {
  expect_snapshot_error(mutation_frequency(data[, 1:8], 3))
})

test_that("error if lack mutation_name column", {
  expect_snapshot_error(mutation_frequency(data[, -4], 3))
})

test_that("output has unique mutation names", {
  out <- mutation_frequency(data, 3)
  expect_setequal(out$mutation_name, unique(out$mutation_name))
})

test_that("result inherits new class", {
  expect_s3_class(mutation_frequency(data, 3), "mut_freq")
})

test_that("results computed correctly", {
  expect_equal(mutation_frequency(data, 5), plot)
})

test_that("properly account for threshold (#35)", {
  data <- new_ref_alt_cov_tbl(tibble::tribble(
    ~sample, ~mutation_name, ~ref_umi_count, ~alt_umi_count, ~coverage,
    "D10-JJJ-30", "crt-Met74Ile", 0, 1, 1,
    "D10-JJJ-15", "crt-Ala220Ser", 9, 3, 12,
  ))

  expect_equal(
    mutation_frequency(data, 5),
    new_mut_freq(tibble::tibble(mutation_name = "crt-Ala220Ser", frequency = 0))
  )
})

# plot_mutation_frequency() Test Cases ----------------------------------------
test_that("data must have mut_freq class", {
  df <- tibble::tibble(a = 1, b = 2)
  expect_error(plot_mutation_frequency(df))
  expect_snapshot_error(plot_mutation_frequency(df))
})

test_that("error if not mut_freq class is pluralized properly", {
  # One class
  expect_error(plot_mutation_frequency("one class"))
  expect_snapshot_error(plot_mutation_frequency("one class"))

  # Multiple classes
  multiple_classes <- tibble::tibble(a = 1)
  expect_error(plot_mutation_frequency("one class"))
  expect_snapshot_error(plot_mutation_frequency(multiple_classes))
})

test_that("creates a nice plot", {
  vdiffr::expect_doppelganger(
    "default plot is informative",
    plot_mutation_frequency(plot)
  )
})

test_that("plot and autoplot methods work", {
  vdiffr::expect_doppelganger("autoplot method works", autoplot(plot))
  vdiffr::expect_doppelganger("plot method works", plot(plot))
})
