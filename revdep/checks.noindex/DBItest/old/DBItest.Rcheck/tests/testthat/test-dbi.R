test_that("Exported DBI methods as expected", {
  skip_if_not_installed("DBI", "1.1.3.9002")

  expect_equal(all_dbi_generics(), fetch_dbi_generics())
})
