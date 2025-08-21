test_that("GuideAxis", {
  if (with_versions(R < "4.2.0")) skip_on_os("linux")
  expect_construct(ggplot2::GuideAxis)
  expect_construct(ggplot2::guide_axis(angle = 90))
  expect_construct(ggplot2::guide_axis(n.dodge = 2))
  expect_construct(ggplot2::guide_axis())
  expect_construct(
    ggplot2::guide_axis(),
    ggplot2::new_guide(
      title = ggplot2::waiver(),
      theme = NULL,
      check.overlap = FALSE,
      angle = ggplot2::waiver(),
      n.dodge = 1,
      minor.ticks = FALSE,
      cap = "none",
      order = 0,
      position = ggplot2::waiver(),
      name = "axis",
      available_aes = c("x", "y", "r"),
      super = ggplot2::GuideAxis
    ) |>
      evalq(constructive::.env("0x123456789", parents = "namespace:ggplot2")),
    opts_GuideAxis("next")
  )
})
