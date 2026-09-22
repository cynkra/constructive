test_that("ecdf", {
  expect_snapshot({
    construct(ecdf(c(1, 2, 2, 3)))
    x <- c(3, 1, NA, 2.5, 3)
    construct(ecdf(x))
    construct(ecdf(1:3))
    construct(ecdf(c(1, 2, 2, 3)), opts_ecdf("next"), opts_environment("list2env"))
    # an ecdf that can't be built with `ecdf()`
    e <- ecdf(c(1, 2, 2, 3))
    environment(e) <- list2env(
      list(f = 0, method = 2L, na.rm = TRUE, nobs = 4L, x = c(1, 2), y = c(0.3, 1), yleft = 0, yright = 1),
      parent = asNamespace("stats")
    )
    construct(e, opts_environment("list2env"))
  })
})
