test_that("data.frame", {
  expect_snapshot({
    # one line, implicit row names
    construct(head(cars,2))
    # multiline, with row names
    construct(head(mtcars,2))
    # row names (numbers) are integer but not  1:n
    construct(tail(cars,2))
    # read.table on num, no row names
    construct(head(cars,2), opts_data.frame(constructor = "read.table"))
    # read.table on num, no row names, one_liner
    construct(head(cars,2), opts_data.frame(constructor = "read.table"), one_liner = TRUE)
    # read.table on num
    construct(transform(mtcars[1:2, 1:2], chr = c("a", "b"), int = 1:2), opts_data.frame(constructor = "read.table"))
    # read.table ignored if unsupported types, e.g. factor
    construct(head(iris,2), opts_data.frame(constructor = "read.table"))
    # read.table ignored if row names are not default
    construct(data.frame(a=1:2, b=3:4)[2,], opts_data.frame("read.table"))
    # handle list  and df cols
    construct(as.data.frame(tibble::tibble(a = 1:2, b = list(3, 4))))
    construct(as.data.frame(tibble::tibble(a = 1:2, b = tibble::tibble(x = 3:4))))
    # handle non syntactic names
    construct(data.frame(a=1, `a a` = 2, check.names = FALSE))
    # handle nas
    construct(data.frame(
      a= c(NA, NA),
      b= c(TRUE, NA),
      c= c(NA_character_, NA),
      d= c("a", NA),
      e= c(NA_integer_, NA),
      f= c(1L, NA),
      g= c(NA_real_, NA),
      h= c(1, NA)
    ))
    # handle AsIs list columns
    construct(data.frame(a = I(list(2))))
    # handle zero row data frame
    construct(data.frame(a = character()))
    # use list constructor
    construct(head(cars,2), opts_data.frame("list"))
    # corrupted df
    construct(structure(
      list(V1 = NULL, V2 = NULL, V3 = NULL, V4 = NULL),
      row.names = c(NA, 0L),
      class = "data.frame"
    ))
    construct(data.frame(a = "two words"), constructive::opts_data.frame("read.table"))
    construct(data.frame(row.names = c("a", "b")))
  })
})

test_that("recycle in data frames", {
  expect_snapshot({
    construct(data.frame(a = 1:2, b = c(1, 1)))
    construct(data.frame(a = c(1, 1), b = c(1, 1)))
    construct(data.frame(a = 1:2, b = factor(c("a", "a"))))
    construct(data.frame(a = 1:2, b = as.Date(c("2000-01-01", "2000-01-01"))))
  })
})

test_that("duplicate names in data frames", {
  expect_snapshot({
    construct(data.frame(a = 1, a =2, check.names = FALSE))
  })
})

test_that("non standard names in data frames", {
  expect_snapshot({
    construct(structure(data.frame(1), names = NULL))
    construct(structure(data.frame(1), names = ""))
    construct(structure(data.frame(1), names = NA))
    construct(structure(data.frame(1), names = "row.names"))
    construct(structure(data.frame(1), names = "check.rows"))
    construct(structure(data.frame(1), names = "check.names"))
    construct(structure(data.frame(1), names = "fix.empty.names"))
    construct(structure(data.frame(1), names = "stringsAsFactors"))
    construct(structure(data.frame(1, 2), names = c("a", "")))
    construct(structure(data.frame(1, 2), names = c("a", NA)))
    construct(structure(data.frame(1, 2), names = c("a", "row.names")))
    construct(structure(data.frame(1, 2), names = c("a", "check.rows")))
    construct(structure(data.frame(1, 2), names = c("a", "check.names")))
    construct(structure(data.frame(1, 2), names = c("a", "fix.empty.names")))
    construct(structure(data.frame(1, 2), names = c("a", "stringsAsFactors")))
  })
})
