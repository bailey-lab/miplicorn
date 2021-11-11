data <- tibble::tribble(
  ~sample, ~ref, ~alt, ~ref_umi_count, ~alt_umi_count,
  "S1", "A", "G", 54, 10,
  "S2", "G", "A", 15, 0,
  "S3", "T", "C", 0, 15,
  "S4", "GA", "G", 2, 18,
  "S5", "G", "ATC", 0, 10
)

res <- tibble::tribble(
  ~sample, ~ref, ~alt, ~ref_umi_count, ~alt_umi_count, ~ans_der_indel,
  "S1", "A", "G", 54, 10, "ref",
  "S2", "G", "A", 15, 0, "ref",
  "S3", "T", "C", 0, 15, "alt",
  "S4", "GA", "G", 2, 18, "del",
  "S5", "G", "ATC", 0, 10, "ins"
)

test_that("labels mutations correctly", {
  expect_equal(label_mutations(data), res)
})

test_that("control position", {
  expect_equal(
    label_mutations(data, .before = ref),
    dplyr::relocate(res, ans_der_indel, .before = ref)
  )
  expect_equal(
    label_mutations(data, .after = sample),
    dplyr::relocate(res, ans_der_indel, .after = sample)
  )
})

test_that("error if control position with both .before and .after", {
  expect_error(label_mutations(data, .before = ref, .after = sample))
  expect_snapshot_error(label_mutations(data, .before = ref, .after = sample))
})
