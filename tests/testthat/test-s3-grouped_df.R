test_that("grouped_df", {
  expect_pipe_snapshot({
    construct(dplyr::group_by(head(cars,2), dist))
  })
})
