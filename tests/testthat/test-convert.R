test_that("empty case", {
  expect_equal(convert_single(""), "")
  expect_equal(convert_three(""), "")
})

test_that("parse multiple amino acids", {
  expect_equal(convert_single("FH"), "PheHis")
  expect_equal(convert_three("PheHis"), "FH")
})

test_that("recongize non amino acids", {
  expect_equal(convert_single("S123P"), "Ser123Pro")
  expect_equal(convert_three("Ser123Pro"), "S123P")
})

test_that("case insensetive", {
  expect_equal(convert_single("G"), "Gly")
  expect_equal(convert_single("g"), "Gly")
  expect_equal(convert_three("GLY"), "G")
  expect_equal(convert_three("GlY"), "G")
  expect_equal(convert_three("gly"), "G")
})

test_that("leaves non amino acid text unaffected", {
  expect_equal(convert_single("123-XZ"), "123-XZ")
  expect_equal(convert_three("123-XZ"), "123-XZ")
  expect_equal(convert_single("S456P-XYZ"), "Ser456Pro-XTyrZ")
})
