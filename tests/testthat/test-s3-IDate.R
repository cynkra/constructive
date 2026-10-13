test_that("IDate", {
  skip_if_not_installed("data.table")
  expect_snapshot({
    construct(data.table::as.IDate("2024-01-01"))
    construct(stats::setNames(data.table::as.IDate(c("2024-01-01", NA, "1900-12-31")), c("a", "b", "c")))
    construct(data.table::as.IDate(NA))
    construct(data.table::as.IDate(character()))
    construct(data.table::as.IDate(c(-1000000L, 1L)))
    construct(data.table::as.IDate("2024-01-01"), opts_IDate("next"))
    construct(data.table::as.IDate("2024-01-01"), opts_IDate("integer"))
  })
})
