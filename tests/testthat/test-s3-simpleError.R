test_that("simpleError", {
  expect_snapshot({
    construct(simpleError("hello"))
    construct(simpleError("hello", call = quote(a())))
  })
})
