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

test_that("matrix with `byrow = TRUE`", {
  expect_snapshot({
    construct(matrix(c(1, 20, 300, 4000, 5, 6), 2), opts_matrix(byrow = TRUE))
    construct(matrix(1:6, 2, dimnames = list(c("a", "b"), c("x", "y", "z"))), opts_matrix(byrow = TRUE))
    construct(matrix(c("a", "bbb", NA, "d"), 2), opts_matrix(byrow = TRUE))
    # typed NAs are kept when all elements are NA
    construct(matrix(NA_real_, 2, 2), opts_matrix(byrow = TRUE))
    construct(matrix(c(TRUE, NA, FALSE, TRUE), 1), opts_matrix(byrow = TRUE))
    construct(structure(matrix(1:4, 2), foo = "bar"), opts_matrix(byrow = TRUE))
    construct(list(m = matrix(1:4, 2)), opts_matrix(byrow = TRUE))
    # ignored for list matrices, empty matrices and one liners
    construct(matrix(list(1, "a", TRUE, NULL), 2), opts_matrix(byrow = TRUE))
    construct(matrix(numeric(0), 0, 3), opts_matrix(byrow = TRUE))
    construct(matrix(1:4, 2), opts_matrix(byrow = TRUE), one_liner = TRUE)
  })
})
