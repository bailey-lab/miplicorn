probes <- tibble::tribble(
  ~chrom, ~start, ~end, ~probe_set,
  "chr14", 2342135L, 2342284L, "IBC",
  "chr3", 830503L, 830769L, "DR2",
  "chr5", 482233L, 482391L, "IBC",
  "chr9", 375274L, 375417L, "IBC",
  "chr12", 532032L, 532281L, "DR2",
  "chr7", 383447L, 383653L, "HAP",
  "chr14", 1401991L, 1402160L, "IBC",
  "chr4", 734737L, 734936L, "HAP",
  "chr10", 93054L, 93223L, "IBC",
  "chr7", 162127L, 162277L, "IBC"
)

test_that("error if wrong mapping package specified", {
  # Silence deprecation warnings
  withr::local_options(lifecycle_verbosity = "quiet")

  # Snapshots
  expect_snapshot(
    error = TRUE,
    chromosome_map(genome_Pf3D7, probes)
  )
  expect_snapshot(
    error = TRUE,
    chromosome_map(genome_Pf3D7, probes, "ggplot2")
  )
})

test_that("chromosome_map is deprecated", {
  expect_snapshot(
    error = TRUE,
    chromosome_map(genome_Pf3D7[, -1], probes, "karyoploteR")
  )
  expect_snapshot(
    error = TRUE,
    chromosome_map(genome_Pf3D7[, -1], probes, "chromoMap")
  )
})
