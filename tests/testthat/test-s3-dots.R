test_that("dots", {
  expect_pipe_snapshot({
    # if dots1 and the evaluation env of `...` is in the same env we have
    # infinite recursion issues so we use `local()`
    dots1 <- local((function(...) environment()$...)(a=x, y))
    construct(dots1, opts_environment("list2env"))
    construct(structure(dots1, class = "foo"), opts_environment("list2env"))

    f <- function(...) {
      y <- 1
      g(y = y, ...)
    }
    g <- function(...) environment()$...
    x <- 1
    dots2 <- local(f(x = x))
    construct(dots2, opts_environment("list2env"))
  })
})
