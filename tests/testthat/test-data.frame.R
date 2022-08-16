test_that("data.frame", {
  expect_snapshot({
    # one line, implicit row names
    construct(head(cars,2))
    # multiline, with row names
    construct(head(mtcars,2))
    # row names (numbers) are integer but not  1:n
    construct(tail(cars,2))
    # read.table on num, no row names
    construct(head(cars,2), read.table = TRUE)
    # read.table on num
    construct(transform(mtcars[1:2, 1:2], chr = c("a", "b"), int = 1:2), read.table = TRUE)
    # read.table ignored if unsupported types, e.g. factor
    construct(head(iris,2), read.table = TRUE)
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
  })
})
