test_that("lintr is happy", {
  skip_on_cran()

  expect_false("package:DBI" %in% search())
  require(DBI)
  on.exit(detach(), add = TRUE)
  expect_true("package:DBI" %in% search())

  # lintr::expect_lint_free()
  detach()
  on.exit(NULL, add = FALSE)
  expect_false("package:DBI" %in% search())
})
