test_that("grouped_df", {
  expect_snapshot({
    construct(dplyr::rowwise(head(cars,2)))
    construct(dplyr::rowwise(head(cars,2), dist))
  })
})
