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
      seq(0, 30, by = 1)
    Code
      construct(c(0, 1:30), one_liner = TRUE)
    Output
      seq(0, 30, by = 1)
    Code
      construct(structure("a", names = ""))
    Output
      "a" |>
        structure(names = "")
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
      c("\U{430}" = 1)
    Code
      construct(c(NaN, NA))
    Output
      c(NaN, NA)
    Code
      construct(c(NaN, NA, NaN))
    Output
      c(NaN, NA, NaN)
    Code
      construct(c(NA, NaN, NA))
    Output
      c(NA, NaN, NA)

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
      c(foo = "a", "a", "b", "c", "c", "c", "c")
    Code
      construct(c("a", "b", "a", "b", "a", "b", "a", "b"))
    Output
      rep(c("a", "b"), 4)
    Code
      construct(c("a", "a", "b", "b", "c", "c"))
    Output
      rep(c("a", "b", "c"), each = 2L)
    Code
      construct(c(1, 2, 3, 4, 1, 2, 3, 4))
    Output
      rep(seq(1, 4, by = 1), 2)
    Code
      construct(as.integer(c(1, 2, 3, 4, 1, 2, 3, 4)))
    Output
      rep(1:4, 2)
    Code
      construct(c(2, 4, 6, 8, 2, 4, 6, 8))
    Output
      rep(seq(2, 8, by = 2), 2)
    Code
      construct(as.integer(c(2, 4, 6, 8, 2, 4, 6, 8)))
    Output
      rep(seq(2L, 8L, by = 2L), 2)
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
      '"hello"'
    Code
      construct("'\"hello\"'", check = FALSE)
    Output
      r"['"hello"']"
    Code
      construct("'\"hello\"'", check = FALSE)
    Output
      r"['"hello"']"
    Code
      construct("\\", check = FALSE)
    Output
      r"[\]"
    Code
      construct("\\\\", check = FALSE)
    Output
      r"[\\]"
    Code
      construct("\n\\")
    Output
      "\n\\"
    Code
      construct("ü", opts_character(unicode_representation = "latin"))
    Output
      "ü"
    Code
      construct("ü", check = FALSE)
    Output
      "\U{FC}"
    Code
      construct("ü\\", opts_character(unicode_representation = "latin", escape = FALSE),
      check = FALSE)
    Output
      r"[ü\]"
    Code
      construct("ü\\", opts_character(escape = FALSE))
    Output
      "\U{FC}\\"
    Code
      construct(c(а = "a"))
    Output
      c("\U{430}" = "a")
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
      construct(NA_complex_)
    Output
      NA_complex_
    Code
      construct(c(NA_complex_, NA_complex_))
    Output
      c(NA_complex_, NA_complex_)
    Code
      construct(c(NA_complex_, NA_complex_, NA_complex_))
    Output
      rep(NA_complex_, 3L)
    Code
      construct(c(NA_complex_, NA_complex_, NA_complex_), opts_atomic(compress = FALSE))
    Output
      c(NA_complex_, NA_complex_, NA_complex_)
    Code
      construct(c(NA_complex_, 1))
    Output
      c(NA_complex_, 1)
    Code
      construct(c(NA_complex_, 0+1i))
    Output
      c(NA_complex_, 1i)
    Code
      construct(1e-10 + 0+1e+10i)
    Output
      1e-10+1e+10i
    Code
      construct(c(1e-10 + 0+1e+10i, 2e-10 + 0+2e+10i))
    Output
      c(1e-10+1e+10i, 2e-10+2e+10i)
    Code
      construct(complex(real = 1, imaginary = NA))
    Output
      complex(real = 1, imaginary = NA)
    Code
      construct(complex(real = 1, imaginary = NaN))
    Output
      complex(real = 1, imaginary = NaN)
    Code
      construct(complex(real = NaN, imaginary = NaN))
    Output
      complex(real = NaN, imaginary = NaN)
    Code
      construct(complex(real = NA, imaginary = NA))
    Output
      NA_complex_
    Code
      construct(complex(real = NA, imaginary = 1))
    Output
      complex(real = NA, imaginary = 1)
    Code
      construct(complex(real = NaN, imaginary = 1))
    Output
      complex(real = NaN, imaginary = 1)
    Code
      construct(c(complex(real = NaN, imaginary = 1), complex(real = NaN, imaginary = 1)))
    Output
      c(complex(real = NaN, imaginary = 1), complex(real = NaN, imaginary = 1))
    Code
      construct(c(1 + 0+1i, complex(real = NaN, imaginary = 1)))
    Output
      c(1+1i, complex(real = NaN, imaginary = 1))

# NA and empty names

    Code
      construct(structure(logical(2), names = c("", "")))
    Output
      c(FALSE, FALSE) |>
        structure(names = c("", ""))
    Code
      construct(structure(logical(2), names = c("", NA)))
    Output
      c(FALSE, FALSE) |>
        structure(names = c("", NA))
    Code
      construct(structure(logical(2), names = c(NA, NA)))
    Output
      c(FALSE, FALSE) |>
        structure(names = c(NA_character_, NA_character_))
    Code
      construct(structure(logical(2), names = c(NA, "a")))
    Output
      c(FALSE, FALSE) |>
        structure(names = c(NA, "a"))
    Code
      construct(structure(logical(2), names = c("", "a")))
    Output
      c(FALSE, a = FALSE)
    Code
      construct(structure(logical(2), names = c("", "a")))
    Output
      c(FALSE, a = FALSE)
    Code
      construct(structure(logical(10), names = c("", "a")))
    Output
      logical(10) |>
        structure(names = rep(c("", "a", NA), c(1L, 1L, 8L)))
    Code
      construct(structure(logical(2), names = structure(c("b", "a"), foo = 1)))
    Output
      c(FALSE, FALSE) |>
        structure(
          names = c("b", "a") |>
            structure(foo = 1)
        )
    Code
      construct(structure(integer(2), names = c("", "")))
    Output
      c(0L, 0L) |>
        structure(names = c("", ""))
    Code
      construct(structure(integer(2), names = c("", NA)))
    Output
      c(0L, 0L) |>
        structure(names = c("", NA))
    Code
      construct(structure(integer(2), names = c(NA, NA)))
    Output
      c(0L, 0L) |>
        structure(names = c(NA_character_, NA_character_))
    Code
      construct(structure(integer(2), names = c(NA, "a")))
    Output
      c(0L, 0L) |>
        structure(names = c(NA, "a"))
    Code
      construct(structure(integer(2), names = c("", "a")))
    Output
      c(0L, a = 0L)
    Code
      construct(structure(integer(10), names = c("", "a")))
    Output
      integer(10) |>
        structure(names = rep(c("", "a", NA), c(1L, 1L, 8L)))
    Code
      construct(structure(integer(2), names = structure(c("b", "a"), foo = 1)))
    Output
      c(0L, 0L) |>
        structure(
          names = c("b", "a") |>
            structure(foo = 1)
        )
    Code
      construct(structure(double(2), names = c("", "")))
    Output
      c(0, 0) |>
        structure(names = c("", ""))
    Code
      construct(structure(double(2), names = c("", NA)))
    Output
      c(0, 0) |>
        structure(names = c("", NA))
    Code
      construct(structure(double(2), names = c(NA, NA)))
    Output
      c(0, 0) |>
        structure(names = c(NA_character_, NA_character_))
    Code
      construct(structure(double(2), names = c(NA, "a")))
    Output
      c(0, 0) |>
        structure(names = c(NA, "a"))
    Code
      construct(structure(double(2), names = c("", "a")))
    Output
      c(0, a = 0)
    Code
      construct(structure(double(10), names = c("", "a")))
    Output
      numeric(10) |>
        structure(names = rep(c("", "a", NA), c(1L, 1L, 8L)))
    Code
      construct(structure(double(2), names = structure(c("b", "a"), foo = 1)))
    Output
      c(0, 0) |>
        structure(
          names = c("b", "a") |>
            structure(foo = 1)
        )
    Code
      construct(structure(complex(2), names = c("", "")))
    Output
      c(0i, 0i) |>
        structure(names = c("", ""))
    Code
      construct(structure(complex(2), names = c("", NA)))
    Output
      c(0i, 0i) |>
        structure(names = c("", NA))
    Code
      construct(structure(complex(2), names = c(NA, NA)))
    Output
      c(0i, 0i) |>
        structure(names = c(NA_character_, NA_character_))
    Code
      construct(structure(complex(2), names = c(NA, "a")))
    Output
      c(0i, 0i) |>
        structure(names = c(NA, "a"))
    Code
      construct(structure(complex(2), names = c("", "a")))
    Output
      c(0i, a = 0i)
    Code
      construct(structure(complex(10), names = c("", "a")))
    Output
      c(0i, 0i, 0i, 0i, 0i, 0i, 0i, 0i, 0i, 0i) |>
        structure(names = rep(c("", "a", NA), c(1L, 1L, 8L)))
    Code
      construct(structure(complex(2), names = structure(c("b", "a"), foo = 1)))
    Output
      c(0i, 0i) |>
        structure(
          names = c("b", "a") |>
            structure(foo = 1)
        )
    Code
      construct(structure(character(2), names = c("", "")))
    Output
      c("", "") |>
        structure(names = c("", ""))
    Code
      construct(structure(character(2), names = c("", NA)))
    Output
      c("", "") |>
        structure(names = c("", NA))
    Code
      construct(structure(character(2), names = c(NA, NA)))
    Output
      c("", "") |>
        structure(names = c(NA_character_, NA_character_))
    Code
      construct(structure(character(2), names = c(NA, "a")))
    Output
      c("", "") |>
        structure(names = c(NA, "a"))
    Code
      construct(structure(character(2), names = c("", "a")))
    Output
      c("", a = "")
    Code
      construct(structure(character(10), names = c("", "a")))
    Output
      character(10) |>
        structure(names = rep(c("", "a", NA), c(1L, 1L, 8L)))
    Code
      construct(structure(character(2), names = structure(c("b", "a"), foo = 1)))
    Output
      c("", "") |>
        structure(
          names = c("b", "a") |>
            structure(foo = 1)
        )
    Code
      construct(structure(raw(2), names = c("", "")))
    Output
      as.raw(c(0x00, 0x00)) |>
        structure(names = c("", ""))
    Code
      construct(structure(raw(2), names = c("", NA)))
    Output
      as.raw(c(0x00, 0x00)) |>
        structure(names = c("", NA))
    Code
      construct(structure(raw(2), names = c(NA, NA)))
    Output
      as.raw(c(0x00, 0x00)) |>
        structure(names = c(NA_character_, NA_character_))
    Code
      construct(structure(raw(2), names = c(NA, "a")))
    Output
      as.raw(c(0x00, 0x00)) |>
        structure(names = c(NA, "a"))
    Code
      construct(structure(raw(2), names = c("", "a")))
    Message
      {constructive} couldn't create code that reproduces perfectly the input
      i Call `construct_issues()` to inspect the last issues
    Output
      as.raw(c(0x00, 0x00))
    Code
      construct(structure(raw(10), names = c("", "a")))
    Output
      raw(10) |>
        structure(names = rep(c("", "a", NA), c(1L, 1L, 8L)))
    Code
      construct(structure(raw(2), names = structure(c("b", "a"), foo = 1)))
    Output
      as.raw(c(0x00, 0x00)) |>
        structure(
          names = c("b", "a") |>
            structure(foo = 1)
        )

# attributes are repaired on length 0 atomics

    Code
      construct(structure(character(0), foo = 1))
    Output
      character(0) |>
        structure(foo = 1)
    Code
      construct(structure(double(0), foo = 1))
    Output
      numeric(0) |>
        structure(foo = 1)
    Code
      construct(structure(integer(0), foo = 1))
    Output
      integer(0) |>
        structure(foo = 1)
    Code
      construct(structure(complex(0), foo = 1))
    Output
      complex(0) |>
        structure(foo = 1)
    Code
      construct(structure(logical(0), foo = 1))
    Output
      logical(0) |>
        structure(foo = 1)
    Code
      construct(structure(raw(0), foo = 1))
    Output
      raw(0) |>
        structure(foo = 1)

# atomic elements named `recursive` or `use.names`

    Code
      construct(structure(logical(1), names = "recursive"))
    Output
      FALSE |>
        structure(names = "recursive")
    Code
      construct(structure(integer(1), names = "recursive"))
    Output
      0L |>
        structure(names = "recursive")
    Code
      construct(structure(numeric(1), names = "recursive"))
    Output
      0 |>
        structure(names = "recursive")
    Code
      construct(structure(complex(1), names = "recursive"))
    Output
      0i |>
        structure(names = "recursive")
    Code
      construct(structure(raw(1), names = "recursive"))
    Output
      as.raw(0x00) |>
        structure(names = "recursive")

# opts_atomic() inheritance

    Code
      construct(c(TRUE, FALSE, TRUE), opts_logical(trim = 1, fill = "+"))
    Message
      {constructive} couldn't create code that reproduces perfectly the input
      i Call `construct_issues()` to inspect the last issues
    Output
      c(TRUE, +2)
    Code
      construct(c(TRUE, FALSE, TRUE), opts_atomic(trim = 0), opts_logical(trim = 1,
        fill = "+"))
    Message
      {constructive} couldn't create code that reproduces perfectly the input
      i Call `construct_issues()` to inspect the last issues
    Output
      c(TRUE, +2)
    Code
      construct(1:3, opts_integer(trim = 1, fill = "+"))
    Message
      {constructive} couldn't create code that reproduces perfectly the input
      i Call `construct_issues()` to inspect the last issues
    Output
      c(1L, +2)
    Code
      construct(1:3, opts_atomic(trim = 0), opts_integer(trim = 1, fill = "+"))
    Message
      {constructive} couldn't create code that reproduces perfectly the input
      i Call `construct_issues()` to inspect the last issues
    Output
      c(1L, +2)
    Code
      construct(c(1, 2, 3), opts_double(trim = 1, fill = "+"))
    Message
      {constructive} couldn't create code that reproduces perfectly the input
      i Call `construct_issues()` to inspect the last issues
    Output
      c(1, +2)
    Code
      construct(c(1, 2, 3), opts_atomic(trim = 0), opts_double(trim = 1, fill = "+"))
    Message
      {constructive} couldn't create code that reproduces perfectly the input
      i Call `construct_issues()` to inspect the last issues
    Output
      c(1, +2)
    Code
      construct(c(0+1i, 0+2i, 0+3i), opts_complex(trim = 1, fill = "+"))
    Message
      {constructive} couldn't create code that reproduces perfectly the input
      i Call `construct_issues()` to inspect the last issues
    Output
      c(1i, +2)
    Code
      construct(c(0+1i, 0+2i, 0+3i), opts_double(trim = 0, fill = "+"))
    Output
      c(1i, 2i, 3i)
    Code
      construct(c(0+1i, 0+2i, 0+3i), opts_atomic(trim = 0), opts_complex(trim = 1,
        fill = "+"))
    Message
      {constructive} couldn't create code that reproduces perfectly the input
      i Call `construct_issues()` to inspect the last issues
    Output
      c(1i, +2)
    Code
      construct(as.raw(c(1, 2, 3)), opts_raw(trim = 1, fill = "+"))
    Message
      {constructive} couldn't create code that reproduces perfectly the input
      i Call `construct_issues()` to inspect the last issues
    Output
      c(as.raw(0x01), +2)
    Code
      construct(as.raw(c(1, 2, 3)), opts_integer(trim = 0))
    Output
      as.raw(c(0x01, 0x02, 0x03))
    Code
      construct(as.raw(c(1, 2, 3)), opts_double(trim = 0), opts_raw(representation = "decimal"))
    Output
      as.raw(c(1, 2, 3))
    Code
      construct(as.raw(c(1, 2, 3)), opts_atomic(trim = 0), opts_raw(trim = 1, fill = "+"))
    Message
      {constructive} couldn't create code that reproduces perfectly the input
      i Call `construct_issues()` to inspect the last issues
    Output
      c(as.raw(0x01), +2)
    Code
      construct(letters, opts_character(trim = 1, fill = "+"))
    Message
      {constructive} couldn't create code that reproduces perfectly the input
      i Call `construct_issues()` to inspect the last issues
    Output
      c("a", +25)
    Code
      construct(letters, opts_atomic(trim = 0), opts_character(trim = 1, fill = "+"))
    Message
      {constructive} couldn't create code that reproduces perfectly the input
      i Call `construct_issues()` to inspect the last issues
    Output
      c("a", +25)
    Code
      construct("🐶", unicode_representation = "ascii")
    Output
      "\U{1F436}"
    Code
      construct("🐶", unicode_representation = "ascii", opts_character(
        unicode_representation = "unicode"))
    Output
      "🐶"
    Code
      construct("🐶", unicode_representation = "unicode", opts_character(
        unicode_representation = "ascii"))
    Output
      "\U{1F436}"
    Code
      construct("🐶", unicode_representation = "ascii", opts_atomic(
        unicode_representation = "unicode"))
    Condition
      Warning:
      `unicode_representation` and `escape` are deprecated in `opts_atomic()`
      i Set those in `opts_character()` instead for the same effect
      i Set those directly in the main function (e.g. `construct()`) to apply them on both character vectors, symbols and argument names
    Output
      "🐶"
    Code
      construct("🐶", unicode_representation = "unicode", opts_atomic(
        unicode_representation = "ascii"))
    Condition
      Warning:
      `unicode_representation` and `escape` are deprecated in `opts_atomic()`
      i Set those in `opts_character()` instead for the same effect
      i Set those directly in the main function (e.g. `construct()`) to apply them on both character vectors, symbols and argument names
    Output
      "\U{1F436}"
    Code
      construct("🐶", opts_atomic(unicode_representation = "ascii"), opts_character(
        unicode_representation = "unicode"))
    Condition
      Warning:
      `unicode_representation` and `escape` are deprecated in `opts_atomic()`
      i Set those in `opts_character()` instead for the same effect
      i Set those directly in the main function (e.g. `construct()`) to apply them on both character vectors, symbols and argument names
    Output
      "🐶"
    Code
      construct("🐶", opts_atomic(unicode_representation = "unicode"), opts_character(
        unicode_representation = "ascii"))
    Condition
      Warning:
      `unicode_representation` and `escape` are deprecated in `opts_atomic()`
      i Set those in `opts_character()` instead for the same effect
      i Set those directly in the main function (e.g. `construct()`) to apply them on both character vectors, symbols and argument names
    Output
      "\U{1F436}"

