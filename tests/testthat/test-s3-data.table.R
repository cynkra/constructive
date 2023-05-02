test_that("data.table", {
  expect_snapshot({
    dt1 <- data.table::data.table(head(cars,2))
    construct(dt1)
    construct(dt1, opts_data.table(selfref = TRUE))
    construct(dt1, opts_data.table("next"))
    construct(dt1, opts_data.table("list"))

    dt2 <- data.table::data.table(dt1, key = "speed")
    construct(dt2)
  })
})
