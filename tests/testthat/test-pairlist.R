test_that("pairlist", {
  expect_snapshot({
    construct(pairlist(a=1, 2))
  })
})
