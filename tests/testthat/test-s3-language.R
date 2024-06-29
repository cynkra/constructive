test_that("language", {
  expect_snapshot({
    construct(quote(a_symbol))
    construct(quote(a + call))
    construct(body(ave))
    construct(quote(expr=))
  })
})

test_that("language after 4.1", {
  # Due to bypass.R
  skip_if(base::`<`(getRversion(), "4.1"))

  expect_snapshot({
    construct(quote(`🐶`))
    construct(quote(`🐶`), unicode_representation = "unicode")
  })
})

test_that("complex language", {
  expect_snapshot({
    x <- quote(a(1)(2))
    attr(x[[1]], "foo") <- "bar"
    construct(x)

    y <- quote(a(1))
    y[[1]] <- c("a", "vector")
    construct(y)
  })
})
