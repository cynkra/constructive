test_that("ggplot2::mapping", {
  skip_if_not_installed("ggplot2")
  skip_if(with_versions(ggplot2 <= "3.5.2"))
  expect_construct(
    ggplot2::aes(mpg ^ 2, wt / cyl)
  )
})
