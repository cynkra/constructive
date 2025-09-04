test_that("data.table", {
  expect_snapshot({
    dt1 <- data.table::data.table(head(cars,2))
    construct(dt1, check = FALSE)
    construct(dt1, opts_data.table(selfref = TRUE), check = FALSE)
    construct(dt1, opts_data.table("next"), check = FALSE)
    construct(dt1, opts_data.table("list"), check = FALSE)

    dt2 <- data.table::data.table(dt1, key = "speed")
    construct(dt2, check = FALSE)
  })
})

test_that("recycle in data tables", {
  expect_snapshot({
    construct(data.table::data.table(a = 1:2, b = c(1, 1)), check = FALSE)
    construct(data.table::data.table(a = c(1, 1), b = c(1, 1)), check = FALSE)
    construct(data.table::data.table(a = 1:2, b = factor(c("a", "a"))), check = FALSE)
    construct(data.table::data.table(a = 1:2, b = as.Date(c("2000-01-01", "2000-01-01"))), check = FALSE)
  })
})

test_that("duplicate names in data tables", {
  expect_snapshot({
    construct(data.table::data.table(a = 1, a =2), check = FALSE)
  })
})

test_that("non standard names in data tables", {
  expect_snapshot({
    construct(structure(data.table::data.table(1), names = NULL), check = FALSE)
    construct(structure(data.table::data.table(1), names = ""), check = FALSE)
    construct(structure(data.table::data.table(1), names = NA), check = FALSE)
    construct(structure(data.table::data.table(1), names = "keep.rownames"), check = FALSE)
    construct(structure(data.table::data.table(1), names = "check.names"), check = FALSE)
    construct(structure(data.table::data.table(1), names = "key"), check = FALSE)
    construct(structure(data.table::data.table(1), names = "stringsAsFactors"), check = FALSE)
    construct(structure(data.table::data.table(1, 2), names = c("a", "")), check = FALSE)
    construct(structure(data.table::data.table(1, 2), names = c("a", NA)), check = FALSE)
    construct(structure(data.table::data.table(1, 2), names = c("a", "keep.rownames")), check = FALSE)
    construct(structure(data.table::data.table(1, 2), names = c("a", "check.names")), check = FALSE)
    construct(structure(data.table::data.table(1, 2), names = c("a", "key")), check = FALSE)
    construct(structure(data.table::data.table(1, 2), names = c("a", "stringsAsFactors")), check = FALSE)
  })
})
