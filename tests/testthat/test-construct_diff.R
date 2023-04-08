test_that("construct_diff", {
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
  })
})
