test_that("S7_object", {
  # FIXME see comment in S/_class tests
  testthat::skip_on_os("linux")
  expect_snapshot({
    construct(S7::S7_object())
    construct(S7::S7_object(), opts_S7_object("next"))
    })
})
