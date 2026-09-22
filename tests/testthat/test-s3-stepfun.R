test_that("stepfun", {
  expect_snapshot({
    construct(stepfun(c(1, 2, 3), c(0, 5, 1, 3)))
    construct(stepfun(1:3, c(0L, 1L, 2L, 3L)))
    construct(stepfun(c(1, 2, 3), c(0, 5, 1, 3), right = TRUE))
    construct(stepfun(c(1, 2, 3), c(0, 5, 1, 3), f = 0.5))
    construct(stepfun(1, c(TRUE, FALSE)))
    construct(structure(stepfun(1, c(0, 1)), call = NULL))
    construct(
      stepfun(c(1, 2, 3), c(0, 5, 1, 3)),
      opts_stepfun("next"),
      opts_environment("list2env")
    )
  })
})
