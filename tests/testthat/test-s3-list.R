test_that("list", {
  # For stability
  .leap.seconds <- as.POSIXct(
    c(
      "1972-07-01", "1973-01-01", "1974-01-01", "1975-01-01", "1976-01-01",
      "1977-01-01", "1978-01-01", "1979-01-01", "1980-01-01", "1981-07-01",
      "1982-07-01", "1983-07-01", "1985-07-01", "1988-01-01", "1990-01-01",
      "1991-01-01", "1992-07-01", "1993-07-01", "1994-07-01", "1996-01-01",
      "1997-07-01", "1999-01-01", "2006-01-01", "2009-01-01", "2012-07-01",
      "2015-07-01", "2017-01-01"
    ),
    tz = "GMT"
  )

  expect_snapshot({
    construct(list(a = 1, b = list(c(1L, 3L), list(.leap.seconds[1:2]))))

    x1 <- as.list(letters[1:4])
    construct(x1)
    construct(x1, opts_list("list2"))

    x2 <- as.list(letters)
    construct(x2)
    construct(x2, opts_list("list2"))

    construct(x2, opts_list(trim = 2)) # fill = "vector"
    construct(x2, opts_list(trim = 26))
    construct(x2, opts_list(trim = 30))
    construct(x2, opts_list(trim = 2, fill = "new_list"))
    construct(x2, opts_list(trim = 2, fill = "+"))
    construct(x2, opts_list(trim = 2, fill = "none"))
    construct(x2, opts_list(trim = 2, fill = "..."))
  })
})
