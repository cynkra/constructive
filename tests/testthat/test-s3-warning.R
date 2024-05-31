test_that("warningCondition", {
  expect_snapshot({
    construct(warningCondition("hello"))
    construct(warningCondition("hello", call = quote(a())))
  })
})
