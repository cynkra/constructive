test_that("CoordRadial", {
  skip_if_not_installed("ggplot2")
  skip_if(with_versions(ggplot2 <= "3.5.2"))
  # examples from ?coord_radial
  expect_construct(ggplot2::coord_radial(theta = "y", expand = FALSE))
  expect_construct(ggplot2::coord_radial(expand = FALSE))
  expect_construct(
    ggplot2::coord_radial("y", start = pi / 3, expand =  FALSE),
    ggplot2::coord_radial(theta = "y", start = 1.0471975511965976, expand = FALSE)
  )
  expect_construct(
    ggplot2::coord_radial(start = -0.4 * pi, end = 0.4 * pi, inner.radius = 0.3),
    ggplot2::coord_radial(start = -1.2566370614359172, end = 1.2566370614359172, inner.radius = 0.3)
  )
  expect_construct(
    ggplot2::coord_radial(
      start = -0.4 * pi,
      end = 0.4 * pi, inner.radius = 0.3,
      thetalim = c(200, 300),
      rlim = c(15, 30),
    ),
    ggplot2::coord_radial(
      start = -1.2566370614359172,
      end = 1.2566370614359172,
      thetalim = c(200, 300),
      rlim = c(15, 30),
      inner.radius = 0.3
    )
  )
})
