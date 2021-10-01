test_that("miplicorn_example() returns example files", {
  expect_equal(
    miplicorn_example("reference_AA_table.csv"),
    system.file("extdata", "reference_AA_table.csv", package = "miplicorn")
  )

  expect_equal(
    miplicorn_example(),
    list.files(system.file("extdata", package = "miplicorn"))
  )
})
