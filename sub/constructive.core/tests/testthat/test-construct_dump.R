test_that("construct_dump", {
  tmp <- tempfile(fileext = ".R")
  construct_dump(list(cars = cars), path = tmp)
  e <- new.env()
  source(tmp, local = e)
  expect_equal(e$cars, cars)
})
