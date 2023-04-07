test_that("labels", {
  expect_snapshot(
    construct(ggplot2::labs(x=1:2,y=c("a", "b")))
  )
})
