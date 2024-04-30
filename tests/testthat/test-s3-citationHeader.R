test_that("citationFooter", {
  expect_snapshot(construct(citHeader("a", "b")))
})
