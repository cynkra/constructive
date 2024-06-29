test_that("array", {
  expect_snapshot({
    construct(as.array(month.abb))
    construct(as.array(month.abb), opts_array("next"))
    construct(array(1:3, c(2,4)))
    construct(structure(1, class = "array"))
    construct(structure(1, class = "array", dim = 1))
  })
})
