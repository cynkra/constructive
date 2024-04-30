test_that("citationFooter", {
  expect_snapshot(construct(citFooter("a", "b")))
})
