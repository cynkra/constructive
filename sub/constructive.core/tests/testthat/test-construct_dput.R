test_that("`construct_dput()`, `construct_base()`, classes arg", {
  expect_snapshot({
    ts_ <- ts(1:10, frequency = 4, start = c(1959, 2))
    construct_dput(ts_)
    construct_base(ts_)
    construct(ts_, classes = "{base}")

    iris2 <- head(iris, 2)
    construct_dput(iris2)
    construct_base(iris2)
    construct(iris2, classes = "{base}")
    construct(iris2, classes = "-{base}")
    construct(iris2, classes = "factor")
    construct(iris2, classes = "-factor")

    construct_dput(dplyr::band_members)
    construct_base(dplyr::band_members)
  })
})
