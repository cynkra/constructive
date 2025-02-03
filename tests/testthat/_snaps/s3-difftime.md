# difftime

    Code
      construct(as.difftime(1, units = "secs"))
    Output
      as.difftime(1, units = "secs")
    Code
      construct(as.difftime(2, units = "mins"))
    Output
      as.difftime(2, units = "mins")
    Code
      x <- structure(c(NA, 74269, 39024, 64597, 24937, NA, NA, 50690, 19113), class = c(
        "foo", "difftime"), units = "secs")
      construct(x)
    Output
      as.difftime(c(NA, 74269, 39024, 64597, 24937, NA, NA, 50690, 19113), units = "secs") |>
        structure(class = c("foo", "difftime"))

