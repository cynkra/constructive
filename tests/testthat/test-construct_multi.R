test_that("construct_multi", {
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

  expect_snapshot(
    construct_multi(list(a = letters, b = .leap.seconds))
  )
  expect_snapshot(
    construct_multi(new_environment(list(a = letters, b = .leap.seconds)))
  )
  expect_error(
    construct_multi(list(letters, .leap.seconds)),
    "named"
  )
  expect_error(
    construct_multi(letters),
    "named"
  )
})
