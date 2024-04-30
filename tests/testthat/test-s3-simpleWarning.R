test_that("simpleWarning", {
  expect_snapshot({
    construct(simpleWarning("hello"))
    construct(simpleWarning("hello", call = quote(a())))
  })
})
