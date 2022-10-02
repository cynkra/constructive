test_that("externalptr", {
  expect_snapshot(suppressWarnings({
    obj <- attributes(data.table::data.table(a=1))
    construct(obj, check = FALSE)
  }))
  expect_warning({
    obj <- attributes(data.table::data.table(a=1))
    construct(obj, check = FALSE)
  }, regexp = "cannot reconstruct pointers")
})
