test_that("miplicorn_example() returns a single example files", {
  expect_equal(
    miplicorn_example("reference_AA_table.csv"),
    system.file("extdata", "reference_AA_table.csv", package = "miplicorn")
  )
})

test_that("miplicorn_examples() returns the example files", {
  expect_equal(
    miplicorn_examples(),
    list.files(system.file("extdata", package = "miplicorn"))
  )
})
