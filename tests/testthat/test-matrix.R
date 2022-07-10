test_that("matrix", {
  expect_snapshot({
    construct(WorldPhones)
    construct(matrix(1:9, 3))
    construct(matrix(1:9, 1))
  })
})
