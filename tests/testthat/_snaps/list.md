# list

    Code
      construct(as.list(letters[1:4]))
    Output
      list("a", "b", "c", "d")
    Code
      construct(list(a = 1, b = list(c(1L, 3L), list(.leap.seconds[1:2]))))
    Output
      list(
        a = 1,
        b = list(c(1L, 3L), list(as.POSIXct(c("1972-07-01", "1973-01-01"), tz = "GMT")))
      )
    Code
      x <- list(1)
      class(x) <- "foo"
      construct(x)
    Output
      list(1) |>
        structure(class = "foo")
    Code
      construct(as.list(letters), max_list = 3)
    Output
      list("a", "b", "c", +23)
    Code
      construct(as.list(letters), max_list = 28)
    Output
      list(
        "a", "b", "c", "d", "e", "f", "g", "h", "i", "j", "k", "l", "m", "n", "o",
        "p", "q", "r", "s", "t", "u", "v", "w", "x", "y", "z"
      )
    Code
      construct(as.list(letters), max_list = 0)
    Output
      list()

