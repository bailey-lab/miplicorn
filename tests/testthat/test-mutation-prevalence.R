ref_alt_cov <- new_ref_alt_cov_tbl(tibble::tribble(
  ~sample, ~gene_id, ~gene, ~mutation_name, ~exonic_func, ~aa_change, ~targeted, ~ref_umi_count, ~alt_umi_count, ~coverage,
  "D10-JJJ-53", "PF3D7_0106300", "atp6", "atp6-Glu431Lys", "missense_variant", "Glu431Lys", "Yes", 0, 77, 77,
  "D10-JJJ-21", "PF3D7_0106300", "atp6", "atp6-Glu431Lys", "missense_variant", "Glu431Lys", "Yes", 0, 619, 619,
  "D10-JJJ-28", "PF3D7_0106300", "atp6", "atp6-Glu431Lys", "missense_variant", "Glu431Lys", "Yes", 0, 145, 145,
  "D10-JJJ-7", "PF3D7_0106300", "atp6", "atp6-Ser466Asn", "missense_variant", "Ser466Asn", "No", 10, 0, 10,
  "D10-JJJ-53", "PF3D7_0106300", "atp6", "atp6-Ser466Asn", "missense_variant", "Ser466Asn", "No", 9, 0, 9,
  "D10-JJJ-6", "PF3D7_0709000", "crt", "crt-Asn326Ser", "missense_variant", "Asn326Ser", "Yes", 100, 0, 100,
  "D10-JJJ-18", "PF3D7_0709000", "crt", "crt-Asn326Ser", "missense_variant", "Asn326Ser", "Yes", 554, 0, 554,
  "D10-JJJ-7", "PF3D7_0709000", "crt", "crt-Asn326Ser", "missense_variant", "Asn326Ser", "Yes", 50, 50, 100,
  "D10-JJJ-51", "PF3D7_0709000", "crt", "crt-Asn326Ser", "missense_variant", "Asn326Ser", "Yes", 60, 12, 72,
))

ref_alt_cov_result <- new_mut_prev(tibble::tribble(
  ~mutation_name, ~n_total, ~n_mutant, ~prevalence,
  "atp6-Glu431Lys", 3L, 3L, 1,
  "atp6-Ser466Asn", 2L, 0, 0,
  "crt-Asn326Ser", 4L, 2L, 0.5,
))

genotype <- new_geno_tbl(tibble::tribble(
  ~sample, ~gene_id, ~gene, ~mutation_name, ~exonic_func, ~aa_change, ~targeted, ~genotype,
  "2766A-EPHI-1", "PF3D7_1343700", "dhps", "dhps-Ala581Gly", "missense_variant", "Ala581Gly", "Yes", 1,
  "1291A-EPHI-1", "PF3D7_0810800", "dhps", "dhps-Ala581Gly", "missense_variant", "Ala581Gly", "Yes", -1,
  "1076-A-EPHI-1", "PF3D7_1343700", "dhps", "dhps-Ala581Gly", "missense_variant", "Ala581Gly", "Yes", 0,
  "4404DT-EPHI-1", "PF3D7_1343700", "k13", "k13-Gly112Glu", "missense_variant", "Gly112Glu", "No", 0,
  "3690T-EPHI-1", "PF3D7_1343700", "k13", "k13-Gly112Glu", "missense_variant", "Gly112Glu", "No", 0,
  "1396A-EPHI-1", "PF3D7_1343700", "k13", "k13-Gly112Glu", "missense_variant", "Gly112Glu", "No", 2,
  "2623A-EPHI-1", "PF3D7_1343700", "k13", "k13-Gly112Glu", "missense_variant", "Gly112Glu", "No", 0,
  "1319A-EPHI-1", "PF3D7_1343700", "k13", "k13-Gly112Glu", "missense_variant", "Gly112Glu", "No", 1,
  "2658A-EPHI-1", "PF3D7_1343700", "k13", "k13-Gly112Glu", "missense_variant", "Gly112Glu", "No", -1,
  "2565A-EPHI-1", "PF3D7_1343700", "k13", "k13-Gly112Glu", "missense_variant", "Gly112Glu", "No", -1
))

genotype_result <- new_mut_prev(tibble::tribble(
  ~mutation_name, ~n_total, ~n_mutant, ~prevalence,
  "dhps-Ala581Gly", 2L, 1L, 0.5,
  "k13-Gly112Glu", 5L, 2L, 0.4,
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
test_that("error if incorrect class", {
  expect_snapshot_error(mutation_prevalence(ref_alt_cov[, 1:5], threshold = 3))
})

test_that("error if lack mutation_name column", {
  expect_snapshot_error(
    mutation_prevalence(ref_alt_cov[, c(1, 6:10)], threshold = 3)
  )
})

test_that("output has unique mutation names", {
  out <- mutation_prevalence(ref_alt_cov, threshold = 3)
  expect_setequal(out$mutation_name, unique(out$mutation_name))
})

test_that("result inherits new class", {
  expect_s3_class(mutation_prevalence(ref_alt_cov, threshold = 3), "mut_prev")
})

test_that("results computed correctly", {
  expect_equal(
    mutation_prevalence(ref_alt_cov, threshold = 5),
    ref_alt_cov_result
  )
  expect_equal(mutation_prevalence(genotype), genotype_result)
})

# plot_mutation_prevalence() Test Cases ----------------------------------------
test_that("data must have mut_prev class", {
  df <- tibble::tibble(a = 1, b = 2)
  expect_error(plot_mutation_prevalence(df))
  expect_snapshot_error(plot_mutation_prevalence(df))
})

test_that("error if not mut_prev class is pluralized properly", {
  # One class
  expect_error(plot_mutation_prevalence("one class"))
  expect_snapshot_error(plot_mutation_prevalence("one class"))

  # Multiple classes
  multiple_classes <- tibble::tibble(a = 1)
  expect_error(plot_mutation_prevalence(multiple_classes))
  expect_snapshot_error(plot_mutation_prevalence(multiple_classes))
})

test_that("creates a nice plot", {
  vdiffr::expect_doppelganger(
    "default plot is informative",
    plot_mutation_prevalence(ref_alt_cov_result)
  )
})

test_that("plot and autoplot methods work", {
  vdiffr::expect_doppelganger(
    "autoplot method works",
    autoplot(ref_alt_cov_result)
  )
  vdiffr::expect_doppelganger(
    "plot method works",
    plot(ref_alt_cov_result)
  )
})
