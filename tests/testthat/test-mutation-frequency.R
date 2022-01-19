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
  "atp6-Gly639Asp", 0.184029185245237,
  "atp6-Ser466Asn", 0.135245901639344,
  "atp6-Ser769Asn", 0,
  "crt-Ala220Ser", 0.202290076335878,
  "crt-Asn326Asp", 0.127799736495389,
  "crt-Asn326Ser", 0.0337078651685393,
  "crt-Asn75Glu", 0.0533790401567091,
  "crt-Cys72Ser", 0.146189905156437,
  "crt-His97Leu", 0,
  "crt-His97Tyr", 0,
  "crt-Ile356Leu", 0.13093220338983,
  "crt-Ile356Thr", 0.0553715097018457,
  "crt-Lys76Thr", 0.200511481118729,
  "crt-Met74Ile", 0.0533790401567091
))

# mutation_frequency() Test Cases ---------------------------------------------
test_that("error if lack alt, coverage columns", {
  expect_snapshot_error(mutation_frequency(data[, 1:8], 3))
})

test_that("error if lack mutation_name column", {
  expect_snapshot_error(mutation_frequency(data[, 6:8], 3))
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

# plot_mutation_frequency() Test Cases ----------------------------------------
test_that("data must have mut_freq class", {
  df <- tibble::tibble(a = 1, b = 2)
  expect_snapshot_error(plot_mutation_frequency(df))
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
