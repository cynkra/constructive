test_that("data.table", {
  expect_snapshot({
    # one line, no row names
    construct(data.table::as.data.table(head(cars,2)))
    # multiline, data.table doesn't support row names
    construct(data.table::as.data.table(head(mtcars,2)))
  })
})
