test_that("errorCondition", {
  expect_snapshot({
    construct(errorCondition("hello"))
    construct(errorCondition("hello", call = quote(a())))
  })
})
