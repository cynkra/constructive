test_that("S7_object", {
  # FIXME see comment in S/_class tests
  testthat::skip_on_os("linux")
  expect_snapshot({
    construct(S7::S7_object())
    })
})

test_that("S7_object, S7 <= 0.2.1", {
  testthat::skip_on_os("linux")
  skip_if_not_installed("S7")
  skip_if(with_versions(S7 > "0.2.1"))
  expect_snapshot({
    construct(S7::S7_object(), opts_S7_object("next"))
  })
})

test_that("S7_object, S7 > 0.2.1", {
  # objects have a "_S7_version" attribute
  testthat::skip_on_os("linux")
  skip_if_not_installed("S7")
  skip_if(with_versions(S7 <= "0.2.1"))
  expect_snapshot({
    construct(S7::S7_object(), opts_S7_object("next"))
  })
})
