test_that("difftime", {
  expect_snapshot({
    construct(as.difftime(1, units = "secs"))
    construct(as.difftime(2, units = "mins"))
    x <- structure(
      c(NA, 74269, 39024, 64597, 24937, NA, NA, 50690, 19113),
      class = c("foo", "difftime"),
      units = "secs"
    )
    construct(x)
  })
})

