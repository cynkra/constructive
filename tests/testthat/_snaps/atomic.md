# numeric

    Code
      construct(10000)
    Output
      10000
    Code
      construct(1e+05)
    Output
      1e+05
    Code
      construct(0.1)
    Output
      0.1
    Code
      construct(0.1)
    Output
      0.1
    Code
      construct(1.1e-15)
    Output
      1.1e-15
    Code
      construct(c(1, 2, 3), opts_atomic(trim = 0))
    Message
      {constructive} couldn't create code that reproduces perfectly the input
      i Call `construct_issues()` to inspect the last issues
    Output
      numeric(0)
    Code
      construct(c(1, 2, 3), opts_atomic(trim = 1))
    Message
      {constructive} couldn't create code that reproduces perfectly the input
      i Call `construct_issues()` to inspect the last issues
    Output
      c(1, numeric(2))
    Code
      construct(c(1, 2, 3), opts_atomic(trim = 2))
    Message
      {constructive} couldn't create code that reproduces perfectly the input
      i Call `construct_issues()` to inspect the last issues
    Output
      c(1, 2, numeric(1))
    Code
      construct(c(1, 2, 3), opts_atomic(trim = 1, fill = "rlang"))
    Message
      {constructive} couldn't create code that reproduces perfectly the input
      i Call `construct_issues()` to inspect the last issues
    Output
      c(1, rlang::new_double(2))
    Code
      construct(c(1, 2, 3), opts_atomic(trim = 1, fill = "+"))
    Message
      {constructive} couldn't create code that reproduces perfectly the input
      i Call `construct_issues()` to inspect the last issues
    Output
      c(1, +2)
    Code
      construct(c(1, 2, 3), opts_atomic(trim = 1, fill = "..."))
    Message
      ! The code built by {constructive} could not be evaluated.
    Output
      c(1, ...)
    Code
      construct(c(1, 2, 3), opts_atomic(trim = 1, fill = "none"))
    Message
      {constructive} couldn't create code that reproduces perfectly the input
      i Call `construct_issues()` to inspect the last issues
    Output
      1
    Code
      construct(0.07)
    Output
      0.07
    Code
      construct(NA_real_)
    Output
      NA_real_
    Code
      construct(c(1, NA_real_))
    Output
      c(1, NA)
    Code
      construct(c(0, 1:30))
    Output
      c(
        0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21,
        22, 23, 24, 25, 26, 27, 28, 29, 30
      )
    Code
      construct(c(0, 1:30), one_liner = TRUE)
    Output
      c(0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30)

# other atomic

    Code
      construct(letters)
    Output
      c(
        "a", "b", "c", "d", "e", "f", "g", "h", "i", "j", "k", "l",
        "m", "n", "o", "p", "q", "r", "s", "t", "u", "v", "w", "x", "y",
        "z"
      )
    Code
      construct(letters, one_liner = TRUE)
    Output
      c("a", "b", "c", "d", "e", "f", "g", "h", "i", "j", "k", "l", "m", "n", "o", "p", "q", "r", "s", "t", "u", "v", "w", "x", "y", "z")
    Code
      construct(letters, opts_atomic(trim = 1, fill = "rlang"))
    Message
      {constructive} couldn't create code that reproduces perfectly the input
      i Call `construct_issues()` to inspect the last issues
    Output
      c("a", rlang::new_character(25))
    Code
      construct(letters, opts_atomic(trim = 1, fill = "+"))
    Message
      {constructive} couldn't create code that reproduces perfectly the input
      i Call `construct_issues()` to inspect the last issues
    Output
      c("a", +25)
    Code
      construct(letters, opts_atomic(trim = 1, fill = "..."))
    Message
      ! The code built by {constructive} could not be evaluated.
    Output
      c("a", ...)
    Code
      construct(letters, opts_atomic(trim = 1, fill = "none"))
    Message
      {constructive} couldn't create code that reproduces perfectly the input
      i Call `construct_issues()` to inspect the last issues
    Output
      "a"

