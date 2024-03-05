test_that("externalptr", {
  expect_pipe_snapshot({
    dt <- data.table::data.table(a = 1)
    class(dt) <- "data.frame"
    construct(dt)

    classed_ptr <- structure(attr(dt, ".internal.selfref"), class = "foo")
    construct(classed_ptr)
  })
})
