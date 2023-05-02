test_that("data.table", {
  expect_snapshot({
    # one line, no row names
    construct(data.table::as.data.table(head(cars,2)))
    #construct(data.table::as.data.table(head(cars,2)), opts_data.table("next"))
    # multiline, data.table doesn't support row names
    construct(data.table::as.data.table(head(mtcars,2)))
    # use list constructor FIXME: find a way to cleanup the outputs with memory addresses
    # construct(data.table::as.data.table(head(cars,2)), opts_data.table("list"))
    construct(data.table::data.table(head(mtcars,2), key = "cyl"))
  })
})
