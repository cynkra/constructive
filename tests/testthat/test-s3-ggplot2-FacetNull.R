test_that("FacetNull", {
  skip_if_not_installed("ggplot2")
  skip_if(with_versions(ggplot2 <= "3.5.2"))
  expect_construct(ggplot2::facet_null(shrink = FALSE))
  expect_construct(ggplot2::facet_null())
})
