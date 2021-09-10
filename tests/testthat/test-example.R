test_that("MIPr_example() returns a single example files", {
  expect_equal(
    MIPr_example("reference_AA_table.csv"),
    system.file("extdata", "reference_AA_table.csv", package = "MIPr")
  )
})

test_that("MIPr_examples() returns the example files", {
  expect_equal(
    MIPr_examples(),
    list.files(system.file("extdata", package = "MIPr"))
  )
})
