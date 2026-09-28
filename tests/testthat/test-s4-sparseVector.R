test_that("sparseVector", {
  skip_if_not_installed("Matrix")
  expect_snapshot({
    construct(Matrix::sparseVector(c(1.5, NA), c(2, 5), 10))
    construct(Matrix::sparseVector(c(1.5, NA), c(2, 5), 10), opts_sparseVector("next"))
    construct(Matrix::sparseVector(c(1L, 3L), c(2L, 5L), 10L))
    construct(Matrix::sparseVector(c(TRUE, NA), c(2, 5), 10))
    construct(Matrix::sparseVector(i = c(2, 5), length = 10))
    construct(Matrix::sparseVector(1i, 2, 3))
    construct(Matrix::sparseVector(numeric(0), integer(0), 0))
  })
})
