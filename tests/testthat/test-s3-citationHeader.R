test_that("citationHeader", {
  skip_if(with_versions(R < "4.4"))
  expect_snapshot(construct(citHeader("a", "b")))
})
