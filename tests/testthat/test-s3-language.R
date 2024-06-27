test_that("language", {
  expect_snapshot({
    construct(quote(a_symbol))
    construct(quote(a + call))
    construct(body(ave))
    construct(quote(expr=))
  })
})

test_that("language", {
  skip_if(getRversion() < "4.1")

  expect_snapshot({
    eval(parse(text = 'construct(quote(`🐶`))'))
    eval(parse(text = 'construct(quote(`🐶`), unicode_representation = "unicode")'))
  })
})

test_that("complex language", {
  expect_pipe_snapshot({
    x <- quote(a(1)(2))
    attr(x[[1]], "foo") <- "bar"
    construct(x)

    y <- quote(a(1))
    y[[1]] <- c("a", "vector")
    construct(y)
  })
})
