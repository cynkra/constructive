test_that("integer64", {
  expect_snapshot({
    construct(bit64::as.integer64(c("-1000", NA, "100000")))
    construct(bit64::as.integer64(NA))
    # NA is stored as the double -0, -1 as NaN, values close to the minimum as negative subnormals
    construct(bit64::as.integer64(c("0", NA, "-1", "-9223372036854775807")))
  })
})
