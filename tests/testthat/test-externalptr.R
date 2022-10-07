test_that("externalptr", {
  expect_snapshot({
    obj <- attributes(data.table::data.table(a=1))
    construct(obj, check = FALSE)
  })
})
