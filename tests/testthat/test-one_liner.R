test_that("one_liner", {
  expect_snapshot({
    construct(head(mtcars,1), one_liner = TRUE)
    construct(structure(1, foo = head(mtcars,1)), one_liner = TRUE)
    construct(letters, one_liner = TRUE)
    construct(letters, one_liner = TRUE, max_atomic = 24)
    construct(c(1,1:30), one_liner = TRUE)
    construct(c(1,1:30), one_liner = TRUE, max_atomic = 24)
  })
})
