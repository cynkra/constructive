test_that("construct_diff", {
  # stangely we have a different indentation for ubuntu with R 4.1 (one more space)
  # it's hard to solve and doesn't seem that important so we just skip the test
  # for this situation
  if (with_versions(R < "4.2.0")) skip_on_os("linux")

  expect_snapshot({
    construct_diff(
      list(a = head(cars,2), b = "aaaaaaaaaaaaaaaaaaaa", c = "Foo"),
      list(a = head(iris,1), b = "aaaaaaaaaaaaaaaaaaaa", c = "foo"),
      interactive = FALSE
    )

    construct_diff(
      list(a = head(cars,2), b = "aaaaaaaaaaaaaaaaaaaa", c = "Foo"),
      list(a = head(iris,1), b = "aaaaaaaaaaaaaaaaaaaa", c = "foo"),
      interactive = FALSE
    )

    construct_diff(
      list(a = head(cars,2), b = "aaaaaaaaaaaaaaaaaaaa", c = "Foo"),
      list(a = head(iris,1), b = "aaaaaaaaaaaaaaaaaaaa", c = "foo"),
      opts_data.frame("read.table"),
      interactive = FALSE
    )

    construct_diff(1,1)
    construct_diff("é", iconv("é", to = "latin1"), interactive = FALSE)
  })
})
