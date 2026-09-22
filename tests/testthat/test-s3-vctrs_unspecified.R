test_that("vctrs_unspecified", {
  skip_if_not_installed("vctrs")
  expect_snapshot({
    construct(vctrs::unspecified(3))
    construct(vctrs::unspecified(3), opts_vctrs_unspecified("next"))
    # empty
    construct(vctrs::unspecified())
    # names are repaired
    construct(structure(c(a = NA, b = NA), class = "vctrs_unspecified"))
    # corrupted: non NA values
    construct(structure(c(NA, TRUE), class = "vctrs_unspecified"))
  })
})
