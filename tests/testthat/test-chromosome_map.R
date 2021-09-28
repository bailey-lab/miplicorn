# General Test Cases -----------------------------------------------------------
test_that("quiet() silences output", {
  expect_silent(quiet(cat("a")))
  expect_silent(quiet(print("b")))
})

test_that("quiet() shows messages, warnings, and errors", {
  expect_message(quiet(message("a")))
  expect_warning(quiet(warning("b")))
  expect_error(quiet(error("c")))
})

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
single_probe <- dplyr::filter(probes, probe_set == "IBC")

test_that("error if packages not intalled", {
  # Trick R to think packages not installed
  mockery::stub(plot_chromoMap, "requireNamespace", FALSE)
  mockery::stub(plot_karyoploteR, "requireNamespace", FALSE)

  # Expect errors
  expect_error(plot_chromoMap(genome_Pf3D7, probes))
  expect_error(plot_karyoploteR(genome_Pf3D7, probes))

  # Snapshots
  expect_snapshot(
    error = TRUE,
    plot_chromoMap(genome_Pf3D7, probes)
  )
  expect_snapshot(
    error = TRUE,
    plot_karyoploteR(genome_Pf3D7, probes)
  )
})

test_that("error if genome is misformatted", {
  # Expect errors
  expect_error(chromosome_map(genome_Pf3D7[, -1], probes, "chromoMap"))
  expect_error(chromosome_map(genome_Pf3D7[, -1], probes, "karyoploteR"))

  # Snapshots
  expect_snapshot(
    error = TRUE,
    chromosome_map(genome_Pf3D7[, -1], probes, "chromoMap")
  )
  expect_snapshot(
    error = TRUE,
    chromosome_map(genome_Pf3D7[, -1], probes, "karyoploteR")
  )
})

test_that("error if probes is misformatted", {
  # Expect errors
  expect_error(chromosome_map(genome_Pf3D7, probes[, -4], "chromoMap"))
  expect_error(chromosome_map(genome_Pf3D7, probes[, -4], "karyoploteR"))

  # Snapshots
  expect_snapshot(
    error = TRUE,
    chromosome_map(genome_Pf3D7, probes[, -4], "chromoMap")
  )
  expect_snapshot(
    error = TRUE,
    chromosome_map(genome_Pf3D7, probes[, -4], "karyoploteR")
  )
})

test_that("error if wrong mapping package specified", {
  # Expect errors
  expect_error(chromosome_map(genome_Pf3D7, probes))
  expect_error(chromosome_map(genome_Pf3D7, probes, "ggplot2"))

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

test_that("chromosome_map works silently", {
  expect_silent(chromosome_map(genome_Pf3D7, probes, "chromoMap"))
  expect_silent(chromosome_map(genome_Pf3D7, probes, "karyoploteR"))
})

# test_that("chromosome_map() leaves directory unchanged", {
#   pre_call <- list.files(tempdir())
#   invisible(chromosome_map(genome_Pf3D7, probes))
#   expect_equal(list.files(tempdir()), pre_call)
# })

# plot_chromoMap() Test Cases --------------------------------------------------
save_widget <- function(widget) {
  html <- tempfile(fileext = ".html")
  png <- tempfile(fileext = ".png")
  htmlwidgets::saveWidget(widget, html)
  webshot2::webshot(html, png)
}

test_that("plot_chromoMap() draws correctly for one probe", {
  # NB tests require webshot2, which is not on cran yet...
  # skip_on_cran()
  skip_on_ci()
  skip_if_not_installed("webshot2")

  single <- plot_chromoMap(genome_Pf3D7, single_probe)
  expect_snapshot_file(save_widget(single), "chromoMap-single-default.png")

  single_col <- plot_chromoMap(genome_Pf3D7, single_probe, colours = "red")
  expect_snapshot_file(save_widget(single_col), "chromoMap-single-colour.png")

  single_col_vec <- plot_chromoMap(genome_Pf3D7, single_probe, colours = c("red"))
  expect_snapshot_file(save_widget(single_col_vec), "chromoMap-single-colour-vec.png")

  single_col_list <- plot_chromoMap(genome_Pf3D7, single_probe, colours = list("red"))
  expect_snapshot_file(save_widget(single_col_list), "chromoMap-single-colour-list.png")
})

test_that("plot_chromoMap() draws correctly for multiple probes", {
  # NB tests require webshot2, which is not on cran yet...
  # skip_on_cran()
  skip_on_ci()
  skip_if_not_installed("webshot2")

  default <- plot_chromoMap(genome_Pf3D7, probes)
  expect_snapshot_file(save_widget(default), "chromoMap-default.png")

  title <- plot_chromoMap(genome_Pf3D7, probes, title = "Example Chromosome Map")
  expect_snapshot_file(save_widget(title), "chromoMap-title.png")

  colours_vec <- plot_chromoMap(
    genome_Pf3D7,
    probes,
    title = "Example Chromosome Map",
    colours = c("#006A8EFF", "#A8A6A7FF", "#B1283AFF")
  )
  expect_snapshot_file(save_widget(colours_vec), "chromoMap-colours-vec.png")

  colours_list <- plot_chromoMap(
    genome_Pf3D7,
    probes,
    title = "Example Chromosome Map",
    colours = list(c("#006A8EFF", "#A8A6A7FF", "#B1283AFF"))
  )
  expect_snapshot_file(save_widget(colours_vec), "chromoMap-colours-list.png")
})

test_that("plot_chromoMap() overrides defaults with user input", {
  # NB tests require webshot2, which is not on cran yet...
  # skip_on_cran()
  skip_on_ci()
  skip_if_not_installed("webshot2")

  no_colour <- plot_chromoMap(genome_Pf3D7, probes, data_based_color_map = F)
  expect_snapshot_file(save_widget(no_colour), "chromoMap-no-colour.png")

  no_legend <- plot_chromoMap(genome_Pf3D7, probes, legend = F)
  expect_snapshot_file(save_widget(no_legend), "chromoMap-no-legend.png")
})

# plot_karyoploteR() Test Cases ------------------------------------------------
test_that("plot_karyoploteR() draws correctly", {
  vdiffr::expect_doppelganger(
    "karyoploteR default plot one probe",
    plot_karyoploteR(genome_Pf3D7, single_probe)
  )

  vdiffr::expect_doppelganger(
    "karyoploteR default plot many probes",
    plot_karyoploteR(genome_Pf3D7, probes)
  )
})

test_that("plot_karyoploteR() accepts colors", {
  vdiffr::expect_doppelganger(
    "karyoploteR default colours",
    plot_karyoploteR(genome_Pf3D7, probes)
  )

  vdiffr::expect_doppelganger(
    "karyoploteR one colour",
    plot_karyoploteR(genome_Pf3D7, single_probe, colours = "red")
  )
  vdiffr::expect_doppelganger(
    "karyoploteR one colour vec",
    plot_karyoploteR(genome_Pf3D7, single_probe, colours = c("red"))
  )
  # Doesn't work with lists
  # vdiffr::expect_doppelganger(
  #   "karyoploteR one colour list",
  #   plot_karyoploteR(genome_Pf3D7, single_probe, colours = list("red"))
  # )

  vdiffr::expect_doppelganger(
    "karyoploteR many colours vec",
    plot_karyoploteR(genome_Pf3D7, probes, colours = c("#006A8EFF", "#A8A6A7FF", "#B1283AFF"))
  )
  # No lists
  # vdiffr::expect_doppelganger(
  #   "karyoploteR many colours list",
  #   plot_karyoploteR(genome_Pf3D7, probes, colours = list(c("#006A8EFF", "#A8A6A7FF", "#B1283AFF")))
  # )
})

test_that("plot_karyoploteR() accepts title", {
  vdiffr::expect_doppelganger(
    "karyoploteR title",
    plot_karyoploteR(genome_Pf3D7, probes, title = "Example Plot")
  )
})

test_that("can manipulate plot_karyoploteR() plot params", {
  vdiffr::expect_doppelganger(
    "karyoploteR plot params",
    plot_karyoploteR(
      genome_Pf3D7, probes,
      data1height = 100,
      right_margin = 3,
      top_margin = 200
    )
  )
})

test_that("can manipulate plot_karyoploteR() tick marks", {
  vdiffr::expect_doppelganger(
    "karyoploteR tick marks",
    plot_karyoploteR(
      genome_Pf3D7, probes,
      add.units = FALSE,
      minor.ticks = FALSE
    )
  )

  vdiffr::expect_doppelganger(
    "karyoploteR minor tick marks",
    plot_karyoploteR(
      genome_Pf3D7, probes,
      minor.tick.dist = 250000,
      minor.tick.col = "blue",
      minor.tick.len = 15,
    )
  )
})

test_that("can manipulate plot_karyoploteR() chrom color", {
  vdiffr::expect_doppelganger(
    "karyoploteR chrom color",
    plot_karyoploteR(genome_Pf3D7, probes, color.schema = "circos")
  )
})

test_that("can manipulate plot_karyoploteR() chrom label size", {
  vdiffr::expect_doppelganger(
    "karyoploteR chrom label size",
    plot_karyoploteR(genome_Pf3D7, probes, cex = 0.2)
  )
})
