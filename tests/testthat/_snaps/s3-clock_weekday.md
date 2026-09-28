# clock_weekday

    Code
      construct(clock::weekday(1))
    Output
      clock::weekday(1)
    Code
      construct(clock::weekday(c(1, 7, NA)))
    Output
      clock::weekday(c(1, 7, NA))
    Code
      construct(clock::weekday(integer()))
    Output
      clock::weekday(numeric(0))
    Code
      construct(clock::weekday(1:2), opts_clock_weekday("next"))
    Output
      1:2 |>
        structure(class = c("clock_weekday", "vctrs_vctr"))
    Code
      construct(clock::weekday(1:2), opts_clock_weekday("integer"))
    Output
      1:2 |>
        structure(class = c("clock_weekday", "vctrs_vctr"))

