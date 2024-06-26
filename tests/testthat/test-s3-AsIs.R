test_that("AsIs", {
  expect_snapshot({
    construct(I(month.abb))
    construct(I(month.abb), opts_AsIs("next"))
    construct(I(head(cars,2)))

    x <- 1
    class(x) <- c("AsIs", "foo")
    construct(x)

    class(x) <- c("foo", "AsIs")
    construct(x)
  })
})
