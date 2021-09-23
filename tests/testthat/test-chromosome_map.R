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

test_that("error if packages not intalled", {
  # Trick R to think packages not installed
  mockery::stub(chromosome_map, "requireNamespace", FALSE)
  expect_error(chromosome_map(genome_Pf3D7, probes))
})

# test_that("chromosome_map() leaves directory unchanged", {
#   pre_call <- list.files(tempdir())
#   invisible(chromosome_map(genome_Pf3D7, probes))
#   expect_equal(list.files(tempdir()), pre_call)
# })

save_widget <- function(widget) {
  html <- tempfile(fileext = ".html")
  png <- tempfile(fileext = ".png")
  htmlwidgets::saveWidget(widget, html)
  webshot2::webshot(html, png)
}

test_that("chromosome_map() draws correctly", {
  # NB tests require webshot2, which is not on cran yet...
  # skip_on_cran()
  skip_on_ci()
  skip_if_not_installed("webshot2")

  default <- chromosome_map(genome_Pf3D7, probes)
  expect_snapshot_file(save_widget(default), "default.png")

  title <- chromosome_map(genome_Pf3D7, probes, title = "Example Chromosome Map")
  expect_snapshot_file(save_widget(title), "title.png")

  colours <- chromosome_map(
    genome_Pf3D7,
    probes,
    title = "Example Chromosome Map",
    colours = list(c("#006A8EFF", "#A8A6A7FF", "#B1283AFF"))
  )
  expect_snapshot_file(save_widget(colours), "colours.png")
})

test_that("chromosome_map() overrides defaults with user input", {
  # NB tests require webshot2, which is not on cran yet...
  # skip_on_cran()
  skip_on_ci()
  skip_if_not_installed("webshot2")

  no_colour <- chromosome_map(genome_Pf3D7, probes, data_based_color_map = F)
  expect_snapshot_file(save_widget(no_colour), "no_colour.png")

  no_legend <- chromosome_map(genome_Pf3D7, probes, legend = F)
  expect_snapshot_file(save_widget(no_legend), "no_legend.png")
})
