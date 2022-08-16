# Date

    Code
      construct(as.Date(.leap.seconds[1:5]))
    Output
      as.Date(c("1972-07-01", "1973-01-01", "1974-01-01", "1975-01-01", "1976-01-01"))
    Code
      construct(as.Date(.leap.seconds[1:10]))
    Output
      as.Date(c(
        "1972-07-01", "1973-01-01", "1974-01-01", "1975-01-01", "1976-01-01",
        "1977-01-01", "1978-01-01", "1979-01-01", "1980-01-01", "1981-07-01"
      ))
    Code
      dates <- as.Date(.leap.seconds[1:5])
      dates[1] <- Inf
      dates[2] <- -Inf
      construct(dates)
    Output
      as.Date(c(Inf, -Inf, 1461, 1826, 2191), origin = "1970-01-01")

