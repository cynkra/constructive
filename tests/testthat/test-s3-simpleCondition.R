test_that("simpleCondition", {
  expect_snapshot({
    construct(simpleCondition("hello"))
    construct(simpleCondition("hello", call = quote(a())))
  })
})
