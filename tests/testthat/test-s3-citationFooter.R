test_that("citationFooter", {
  skip_if(with_versions(R >= "4.4"))
  expect_snapshot(construct(citFooter("a", "b")))
})
