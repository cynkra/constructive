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
      ! Due to error: '...' used in an incorrect context
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
      seq(0, 30, "by" = 1)
    Code
      construct(c(0, 1:30), one_liner = TRUE)
    Output
      seq(0, 30, "by" = 1)
    Code
      construct(structure("a", names = ""))
    Output
      "a" |>
        structure("names" = "")
    Code
      construct(NaN)
    Output
      NaN
    Code
      construct(c(1, NaN))
    Output
      c(1, NaN)
    Code
      construct(c(а = 1))
    Output
      c(`\xd0\xb0` = 1)

# other atomic

    Code
      construct(letters)
    Output
      c(
        "a", "b", "c", "d", "e", "f", "g", "h", "i", "j", "k", "l", "m", "n", "o",
        "p", "q", "r", "s", "t", "u", "v", "w", "x", "y", "z"
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
      ! Due to error: '...' used in an incorrect context
    Output
      c("a", ...)
    Code
      construct(letters, opts_atomic(trim = 1, fill = "none"))
    Message
      {constructive} couldn't create code that reproduces perfectly the input
      i Call `construct_issues()` to inspect the last issues
    Output
      "a"

# simplify atomic

    Code
      construct(c("a", "a", "b", "c", "c", "c", "c"))
    Output
      rep(c("a", "b", "c"), c(2L, 1L, 4L))
    Code
      construct(c(foo = "a", "a", "b", "c", "c", "c", "c"))
    Output
      c("foo" = "a", "a", "b", "c", "c", "c", "c")
    Code
      construct(c("a", "b", "a", "b", "a", "b", "a", "b"))
    Output
      rep(c("a", "b"), 4)
    Code
      construct(c("a", "a", "b", "b", "c", "c"))
    Output
      rep(c("a", "b", "c"), "each" = 2L)
    Code
      construct(c(1, 2, 3, 4, 1, 2, 3, 4))
    Output
      rep(seq(1, 4, "by" = 1), 2)
    Code
      construct(as.integer(c(1, 2, 3, 4, 1, 2, 3, 4)))
    Output
      rep(1:4, 2)
    Code
      construct(c(2, 4, 6, 8, 2, 4, 6, 8))
    Output
      rep(seq(2, 8, "by" = 2), 2)
    Code
      construct(as.integer(c(2, 4, 6, 8, 2, 4, 6, 8)))
    Output
      rep(seq(2L, 8L, "by" = 2L), 2)
    Code
      construct(c("a", "a", "b", "c", "c", "c", "c"), opts_atomic(compress = FALSE))
    Output
      c("a", "a", "b", "c", "c", "c", "c")
    Code
      construct(c(0L, 0L, -1L, .Machine$integer.max))
    Output
      c(0L, 0L, -1L, 2147483647L)

# character

    Code
      construct("'hello'")
    Output
      "'hello'"
    Code
      construct("\"hello\"")
    Output
      "\"hello\""
    Code
      construct("'\"hello\"'", check = FALSE)
    Output
      "'\"hello\"'"
    Code
      construct("'\"hello\"'", check = FALSE)
    Output
      "'\"hello\"'"
    Code
      construct("\\", check = FALSE)
    Output
      "\\"
    Code
      construct("\\\\", check = FALSE)
    Output
      "\\\\"
    Code
      construct("\n\\")
    Output
      "\n\\"
    Code
      construct("ü", opts_atomic(unicode_representation = "latin"))
    Output
      "\xc3\xbc"
    Code
      construct("ü", check = FALSE)
    Output
      "\xc3\xbc"
    Code
      construct("ü\\", opts_atomic(unicode_representation = "latin", escape = FALSE),
      check = FALSE)
    Output
      "\xc3\xbc\\"
    Code
      construct("ü\\", opts_atomic(escape = FALSE))
    Output
      "\xc3\xbc\\"
    Code
      construct(c(а = "a"))
    Output
      c(`\xd0\xb0` = "a")
    Code
      construct("'\"\n")
    Output
      "'\"\n"

# negative zeroes

    Code
      construct(-0)
    Output
      -0
    Code
      construct(c(-0, -0, -0))
    Output
      -numeric(3)
    Code
      construct(c(0, -0, -0))
    Output
      c(0, -0, -0)

# complex

    Code
      NULL
    Output
      NULL

