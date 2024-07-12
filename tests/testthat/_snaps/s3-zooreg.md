# zooreg

    Code
      construct(zoo::zooreg(1:10, frequency = 4, start = c(1959, 2)))
    Output
      zoo::zooreg(1:10, start = zoo::as.yearqtr("1959 Q2"), frequency = 4)
    Code
      construct(zoo::zoo(1:10, zoo::yearqtr(seq(1959.25, 1961.5, by = 0.25)),
      frequency = 4))
    Output
      zoo::zooreg(1:10, start = zoo::as.yearqtr("1959 Q2"), frequency = 4)
    Code
      construct(zoo::zooreg(1:5, start = as.Date("2000-01-01")))
    Output
      zoo::zooreg(1:5, start = as.Date("2000-01-01"), frequency = 1)
    Code
      construct(zoo::zooreg(1:5, end = zoo::yearmon(2000)))
    Output
      zoo::zooreg(1:5, start = zoo::as.yearmon("Jan 1996"), frequency = 1)

