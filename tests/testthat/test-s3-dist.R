test_that("dist", {
  expect_snapshot({
    construct(as.dist(matrix(c(0, 1, 2, 1, 0, 3, 2, 3, 0), 3)))
    construct(dist(matrix(c(1, 2, 4, 3, 5, 6), 3)))
    m <- matrix(c(1, 2, NA, 4, 5, 6), 3, dimnames = list(c("a", "b", "c"), NULL))
    construct(dist(m, method = "manhattan", diag = TRUE, upper = TRUE))
    construct(dist(matrix(1:2, 1)))
    construct(structure(as.dist(matrix(c(0, 1, 1, 0), 2)), call = NULL))
    construct(dist(matrix(c(1, 2, 4, 3, 5, 6), 3)), opts_dist("next"))
  })
})
