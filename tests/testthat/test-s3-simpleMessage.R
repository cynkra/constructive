test_that("simpleMessage", {
  expect_snapshot({
    construct(simpleMessage("hello"))
    construct(simpleMessage("hello", call = quote(a())))
  })
})
