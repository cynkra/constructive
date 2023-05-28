# list

    Code
      construct(list(a = 1, b = list(c(1L, 3L), list(.leap.seconds[1:2]))))
    Output
      list(
        a = 1,
        b = list(c(1L, 3L), list(as.POSIXct(c("1972-07-01", "1973-01-01"), tz = "GMT")))
      )
    Code
      x1 <- as.list(letters[1:4])
      construct(x1)
    Output
      list("a", "b", "c", "d")
    Code
      construct(x1, opts_list("list2"))
    Output
      rlang::list2("a", "b", "c", "d")
    Code
      x2 <- as.list(letters)
      construct(x2)
    Output
      list(
        "a", "b", "c", "d", "e", "f", "g", "h", "i", "j", "k", "l", "m", "n", "o",
        "p", "q", "r", "s", "t", "u", "v", "w", "x", "y", "z"
      )
    Code
      construct(x2, opts_list("list2"))
    Output
      rlang::list2(
        "a", "b", "c", "d", "e", "f", "g", "h", "i", "j", "k", "l", "m", "n", "o",
        "p", "q", "r", "s", "t", "u", "v", "w", "x", "y", "z",
      )
    Code
      construct(x2, opts_list(trim = 2))
    Message
      {constructive} couldn't create code that reproduces perfectly the input
      i Call `construct_issues()` to inspect the last issues
    Output
      c(list("a", "b"), vector("list", 24))
    Code
      construct(x2, opts_list(trim = 26))
    Output
      list(
        "a", "b", "c", "d", "e", "f", "g", "h", "i", "j", "k", "l", "m", "n", "o",
        "p", "q", "r", "s", "t", "u", "v", "w", "x", "y", "z"
      )
    Code
      construct(x2, opts_list(trim = 30))
    Output
      list(
        "a", "b", "c", "d", "e", "f", "g", "h", "i", "j", "k", "l", "m", "n", "o",
        "p", "q", "r", "s", "t", "u", "v", "w", "x", "y", "z"
      )
    Code
      construct(x2, opts_list(trim = 2, fill = "new_list"))
    Message
      {constructive} couldn't create code that reproduces perfectly the input
      i Call `construct_issues()` to inspect the last issues
    Output
      c(list("a", "b"), rlang::new_list(24))
    Code
      construct(x2, opts_list(trim = 2, fill = "+"))
    Message
      {constructive} couldn't create code that reproduces perfectly the input
      i Call `construct_issues()` to inspect the last issues
    Output
      list("a", "b", +24)
    Code
      construct(x2, opts_list(trim = 2, fill = "none"))
    Message
      {constructive} couldn't create code that reproduces perfectly the input
      i Call `construct_issues()` to inspect the last issues
    Output
      list("a", "b")
    Code
      construct(x2, opts_list(trim = 2, fill = "..."))
    Message
      ! The code built by {constructive} could not be evaluated.
      ! Due to error: '...' used in an incorrect context
    Output
      list("a", "b", ...)

