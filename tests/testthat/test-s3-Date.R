test_that("Date", {
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

  expect_pipe_snapshot({
    construct(structure(19469, class = "Date"))
    construct(structure(19469, class = "Date"), opts_Date("next"))
    construct(structure(19469, class = "Date"), opts_Date("atomic"))
    construct(structure(19469L, class = "Date"))
    construct(structure("19469", class = "Date"))
    # one line
    construct(as.Date(.leap.seconds[1:5]))
    construct(as.Date(.leap.seconds[1:5]), opts_Date("as_date"))
    construct(as.Date(.leap.seconds[1:5]), opts_Date("date"))
    construct(as.Date(.leap.seconds[1:5]), opts_Date("new_date"))
    construct(as.Date(.leap.seconds[1:5]), opts_Date("as.Date.numeric"))
    construct(as.Date(.leap.seconds[1:5]), opts_Date("as_date.numeric"))
    construct(as.Date(.leap.seconds[1:5]), opts_Date("as.Date.numeric", origin = "2000-01-01"))
    construct(as.Date(.leap.seconds[1:5]), opts_Date("as_date.numeric", origin = "2000-01-01"))
    # multiline,
    construct(as.Date(.leap.seconds[1:10]))
    # handle infinite dates
    dates <- as.Date(.leap.seconds[1:5])
    dates[1] <- Inf
    dates[2] <- -Inf
    construct(dates)
    construct(dates, opts_Date("as_date"))
    construct(dates, opts_Date("date"))
    construct(dates, opts_Date("new_date"))
    construct(dates, opts_Date(origin = "2000-01-01"))
    construct(dates, opts_Date("as_date", origin = "2000-01-01"))
    construct(dates, opts_Date("date", origin = "2000-01-01"))
  })
})
