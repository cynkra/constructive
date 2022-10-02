test_that("one_liner", {
  expect_snapshot({
    construct(head(mtcars,1), one_liner = TRUE)
    construct(structure(1, foo = head(mtcars,1)), one_liner = TRUE)
  })
})
