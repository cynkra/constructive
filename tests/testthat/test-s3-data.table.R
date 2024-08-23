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

test_that("recycle in data tables", {
  expect_snapshot({
    construct(data.table::data.table(a = 1:2, b = c(1, 1)))
    construct(data.table::data.table(a = c(1, 1), b = c(1, 1)))
    construct(data.table::data.table(a = 1:2, b = factor(c("a", "a"))))
    construct(data.table::data.table(a = 1:2, b = as.Date(c("2000-01-01", "2000-01-01"))))
  })
})

test_that("duplicate names in data tables", {
  expect_snapshot({
    construct(data.table::data.table(a = 1, a =2))
  })
})
