x <- quote({
  1+1
  2
})
test_that("srcref", {
  expect_snapshot({
    construct(attributes(x), opts_environment(predefine = TRUE))
  })
})
