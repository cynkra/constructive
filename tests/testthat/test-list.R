test_that("list", {
  expect_snapshot({
    construct(as.list(letters[1:4]))
    construct(list(a = 1, b = list(c(1L, 3L), list(.leap.seconds[1:2]))))
    x <- list(1)
    class(x) <- "foo"
    construct(x)
  })
})
