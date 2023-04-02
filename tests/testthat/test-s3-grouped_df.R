test_that("grouped_df", {
  expect_snapshot({
    construct(dplyr::group_by(head(cars,2), dist))
  })
})
