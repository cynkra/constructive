test_that("citationHeader", {
  expect_snapshot(construct(citHeader("a", "b")))
})
