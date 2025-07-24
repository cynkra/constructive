test_that("language", {
  expect_snapshot({
    construct(quote(a_symbol))
    construct(as.symbol("a\\b"))
    construct(quote(a + call))
    construct(quote(expr=))
    construct(as.call(list(quote(expr = ))))
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

test_that("We can construct calls with empty callers", {
  expect_snapshot({
    construct(substitute(X(), list(X = quote(expr = ))))
    construct(substitute({X(1, 2)}, list(X = quote(expr = ))))
  })
})

test_that("We can construct calls with non syntactic literals", {
  expect_snapshot({
    construct(call("fun", -1))
    construct(call("fun", 1+0i))
    construct(call("fun", quote(expr=)))
    construct(call("+", quote(expr=)))
  })
})
