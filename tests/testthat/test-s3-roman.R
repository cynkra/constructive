test_that("roman", {
  expect_snapshot({
    # 3899 is the largest value `as.roman()` accepts in every supported R
    # version; the upper bound was raised to 3999 only in R 4.3
    construct(as.roman(c(1, 12, NA, 3899)))
    construct(as.roman(integer()))
    construct(structure(as.roman(c(1, 12)), names = c("a", "b")))
    # out of range values can't be built with `as.roman()`
    construct(structure(c(1L, 4000L), class = "roman"))
    construct(as.roman(c(1, 12, NA)), opts_roman("next"))
  })
})
