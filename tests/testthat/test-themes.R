test_that("themes are stable", {
  df <- tibble::tibble(x = 1:3, y = 1:3, z = c("a", "b", "a"), a = 1)
  plot <- ggplot2::ggplot(df, ggplot2::aes(x, y, colour = z)) +
    ggplot2::geom_point()

  vdiffr::expect_doppelganger("default_theme", plot + default_theme())
  vdiffr::expect_doppelganger("theme_miplicorn", plot + theme_miplicorn())

  vdiffr::expect_doppelganger("rainbow_theme", plot + rainbow_theme())
  vdiffr::expect_doppelganger("theme_rainbow", plot + theme_rainbow())
})
