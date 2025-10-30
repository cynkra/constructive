test_that("construct_signature", {
  expect_snapshot({
    construct_signature(transform)
    construct_signature(lm)
  })
})
