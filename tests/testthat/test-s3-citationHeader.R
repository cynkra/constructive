test_that("citationFooter", {
  skip_if(with_versions(R != "4.3.0"))
  expect_snapshot(construct(citHeader("a", "b")))
})
