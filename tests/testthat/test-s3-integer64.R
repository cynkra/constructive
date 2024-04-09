test_that("integer64", {
  expect_snapshot({
    construct(bit64::as.integer64(c("-1000", "NA", "100000")))
  })
})
