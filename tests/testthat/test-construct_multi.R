test_that("construct_multi", {
  expect_snapshot(
    construct_multi(list(a=letters, b = .leap.seconds))
  )
  expect_error(
    construct_multi(list(letters, .leap.seconds)),
    "named"
  )
  expect_error(
    construct_multi(letters),
    "named"
  )
})
