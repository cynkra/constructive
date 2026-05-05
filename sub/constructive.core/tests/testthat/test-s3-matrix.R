test_that("matrix", {
  expect_snapshot({
    construct(WorldPhones)
    construct(matrix(1:9, 3))
    construct(matrix(1:9, 1))
    construct(matrix(1:9, 3), opts_matrix("array"))
    construct(matrix(1:9, 3), opts_matrix("next"))
  })
})

test_that("classed matrix", {
  expect_snapshot({
    construct(structure(matrix(1:9, 3), class = "a"))
  })
})

test_that("matrix with rbind and cbind", {
  expect_snapshot({
    construct(matrix(1:4, 2), opts_matrix("cbind"))
    construct(matrix(1:4, 2, dimnames = list(c("a", "b"), c("c", "d"))), opts_matrix("cbind"))
    construct(matrix(1:4, 2, dimnames = list(c("a", "b"))), opts_matrix("cbind"))
    construct(matrix(1:4, 2, dimnames = list(NULL, c("c", "d"))), opts_matrix("cbind"))

    construct(matrix(1:4, 2), opts_matrix("rbind"))
    construct(matrix(1:4, 2, dimnames = list(c("a", "b"), c("c", "d"))), opts_matrix("rbind"))
    construct(matrix(1:4, 2, dimnames = list(c("a", "b"))), opts_matrix("rbind"))
    construct(matrix(1:4, 2, dimnames = list(NULL, c("c", "d"))), opts_matrix("rbind"))
  })
})
