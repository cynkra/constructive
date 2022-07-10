test_that("language", {
  expect_snapshot({
    construct(quote(a_symbol))
    construct(quote(a + call))
    construct(body(ave))
    construct(quote(expr=))
  })
})

