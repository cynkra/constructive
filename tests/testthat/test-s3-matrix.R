test_that("matrix", {
  expect_pipe_snapshot({
    construct(WorldPhones)
    construct(matrix(1:9, 3))
    construct(matrix(1:9, 1))
    construct(matrix(1:9, 3), opts_matrix("array"))
    construct(matrix(1:9, 3), opts_matrix("next"))
    construct(matrix(1:9, 3), opts_matrix("atomic"))
  })
})
