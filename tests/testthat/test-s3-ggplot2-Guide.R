test_that("Guide", {
  expect_construct(
    ggplot2::new_guide(
      n.dodge = 2,
      minor.ticks = TRUE,
      super = ggplot2::GuideAxis
    )
  )
})
