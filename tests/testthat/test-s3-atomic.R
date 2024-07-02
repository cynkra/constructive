test_that("numeric", {
  expect_snapshot({
    # by default no scientific notation
    construct(10000)
    # by default scientific notation
    construct(100000)
    # not truncated
    construct(.1000000000000001)
    # truncated
    construct(.10000000000000001)
    # by default scientific notation
    construct(.0000000000000011)
    # trim
    construct(c(1, 2, 3), opts_atomic(trim = 0))
    construct(c(1, 2, 3), opts_atomic(trim = 1))
    construct(c(1, 2, 3), opts_atomic(trim = 2))
    construct(c(1, 2, 3), opts_atomic(trim = 1, fill = "rlang"))
    construct(c(1, 2, 3), opts_atomic(trim = 1, fill = "+"))
    construct(c(1, 2, 3), opts_atomic(trim = 1, fill = "..."))
    construct(c(1, 2, 3), opts_atomic(trim = 1, fill = "none"))
    # don't print useless extra digits (thanks to format(x, digits = 15))
    construct(0.07)
    construct(NA_real_)
    construct(c(1, NA_real_))
    # one_liner
    construct(c(0, 1:30))
    construct(c(0, 1:30), one_liner = TRUE)
    # empty names
    construct(structure("a", names = ""))
    construct(NaN)
    construct(c(1, NaN))
    construct(c("\U{430}" = 1))

    construct(c(NaN, NA))
    construct(c(NaN, NA, NaN))
    construct(c(NA, NaN, NA))

  })
})

test_that("other atomic", {
  expect_snapshot({
    construct(letters)
    construct(letters, one_liner = TRUE)
    construct(letters, opts_atomic(trim = 1, fill = "rlang"))
    construct(letters, opts_atomic(trim = 1, fill = "+"))
    construct(letters, opts_atomic(trim = 1, fill = "..."))
    construct(letters, opts_atomic(trim = 1, fill = "none"))
  })
})


test_that("simplify atomic", {
  expect_snapshot({
    construct(c("a", "a", "b", "c", "c", "c", "c"))
    construct(c(foo = "a", "a", "b", "c", "c", "c", "c"))
    construct(c("a", "b", "a", "b","a", "b","a", "b"))
    construct(c("a", "a", "b", "b", "c", "c"))
    construct(c(1, 2, 3, 4, 1, 2, 3, 4))
    construct(as.integer(c(1, 2, 3, 4, 1, 2, 3, 4)))
    construct(c(2, 4, 6, 8, 2, 4, 6, 8))
    construct(as.integer(c(2, 4, 6, 8, 2, 4, 6, 8)))
    construct(c("a", "a", "b", "c", "c", "c", "c"), opts_atomic(compress = FALSE))
    construct(c(0L, 0L, -1L, .Machine$integer.max))
  })
})

test_that("character", {
  # check = FALSE for raw strings to pass tests on older R versions
  expect_snapshot({
    construct("'hello'")
    construct('"hello"')
    construct("'\"hello\"'", check = FALSE)
    construct("'\"hello\"'", check = FALSE)
    construct("\\", check = FALSE)
    construct("\\\\", check = FALSE)
    construct("\n\\")
    construct("ü", opts_character(unicode_representation = "latin"))
    construct("ü", check = FALSE)
    construct("ü\\", opts_character(unicode_representation = "latin", escape = FALSE), check = FALSE)
    construct("ü\\", opts_character(escape = FALSE))
    construct(c("\U{430}" = "a"))
    construct("'\"\n")
  })
})

test_that("negative zeroes", {
  expect_snapshot({
    construct(-0)
    construct(c(-0, -0, -0))
    construct(c(0, -0, -0))
    # construct(-NA_real_)
    # construct(-NaN)
  })
})

test_that("complex", {
  expect_snapshot({
    construct(NA_complex_)
    construct(c(NA_complex_, NA_complex_))
    construct(c(NA_complex_, NA_complex_, NA_complex_))
    construct(c(NA_complex_, NA_complex_, NA_complex_), opts_atomic(compress = FALSE))
    construct(c(NA_complex_, 1))
    construct(c(NA_complex_, 1i))
    construct(1e-10 + 1e10i)
    construct(c(1e-10 + 1e10i, 2e-10 + 2e10i))
    construct(complex(real = 1, imaginary = NA))
    construct(complex(real = 1, imaginary = NaN))
    construct(complex(real = NaN, imaginary = NaN))
    construct(complex(real = NA, imaginary = NA))
    construct(complex(real = NA, imaginary = 1))
    construct(complex(real = NaN, imaginary = 1))
    construct(c(complex(real = NaN, imaginary = 1), complex(real = NaN, imaginary = 1)))
    construct(c(1 + 1i, complex(real = NaN, imaginary = 1)))
  })
})


test_that("NA and empty names", {
  expect_snapshot({
    construct(structure(logical(2), names = c("", "")))
    construct(structure(logical(2), names = c("", NA)))
    construct(structure(logical(2), names = c(NA, NA)))
    construct(structure(logical(2), names = c(NA, "a")))
    construct(structure(logical(2), names = c("", "a")))
    construct(structure(logical(2), names = c("", "a")))
    construct(structure(logical(10), names = c("", "a")))
    construct(structure(logical(2), names = structure(c("b", "a"), foo = 1)))

    construct(structure(integer(2), names = c("", "")))
    construct(structure(integer(2), names = c("", NA)))
    construct(structure(integer(2), names = c(NA, NA)))
    construct(structure(integer(2), names = c(NA, "a")))
    construct(structure(integer(2), names = c("", "a")))
    construct(structure(integer(10), names = c("", "a")))
    construct(structure(integer(2), names = structure(c("b", "a"), foo = 1)))

    construct(structure(double(2), names = c("", "")))
    construct(structure(double(2), names = c("", NA)))
    construct(structure(double(2), names = c(NA, NA)))
    construct(structure(double(2), names = c(NA, "a")))
    construct(structure(double(2), names = c("", "a")))
    construct(structure(double(10), names = c("", "a")))
    construct(structure(double(2), names = structure(c("b", "a"), foo = 1)))

    construct(structure(complex(2), names = c("", "")))
    construct(structure(complex(2), names = c("", NA)))
    construct(structure(complex(2), names = c(NA, NA)))
    construct(structure(complex(2), names = c(NA, "a")))
    construct(structure(complex(2), names = c("", "a")))
    construct(structure(complex(10), names = c("", "a")))
    construct(structure(complex(2), names = structure(c("b", "a"), foo = 1)))

    construct(structure(character(2), names = c("", "")))
    construct(structure(character(2), names = c("", NA)))
    construct(structure(character(2), names = c(NA, NA)))
    construct(structure(character(2), names = c(NA, "a")))
    construct(structure(character(2), names = c("", "a")))
    construct(structure(character(10), names = c("", "a")))
    construct(structure(character(2), names = structure(c("b", "a"), foo = 1)))

    construct(structure(raw(2), names = c("", "")))
    construct(structure(raw(2), names = c("", NA)))
    construct(structure(raw(2), names = c(NA, NA)))
    construct(structure(raw(2), names = c(NA, "a")))
    construct(structure(raw(2), names = c("", "a")))
    construct(structure(raw(10), names = c("", "a")))
    construct(structure(raw(2), names = structure(c("b", "a"), foo = 1)))
  })
})

test_that("attributes are repaired on length 0 atomics", {
  expect_snapshot({
    construct(structure(character(0), foo = 1))
    construct(structure(double(0), foo = 1))
    construct(structure(integer(0), foo = 1))
    construct(structure(complex(0), foo = 1))
    construct(structure(logical(0), foo = 1))
    construct(structure(raw(0), foo = 1))
  })
})

test_that("atomic elements named `recursive` or `use.names`", {
  expect_snapshot({
    construct(structure(logical(1), names = "recursive"))
    construct(structure(integer(1), names = "recursive"))
    construct(structure(numeric(1), names = "recursive"))
    construct(structure(complex(1), names = "recursive"))
    construct(structure(raw(1), names = "recursive"))
  })
})

test_that("opts_atomic() inheritance", {
  expect_snapshot({
    construct(c(TRUE, FALSE, TRUE), opts_logical(trim = 1, fill = "+"))
    construct(c(TRUE, FALSE, TRUE), opts_atomic(trim = 0), opts_logical(trim = 1, fill = "+"))

    construct(1:3, opts_integer(trim = 1, fill = "+"))
    construct(1:3, opts_atomic(trim = 0), opts_integer(trim = 1, fill = "+"))

    construct(c(1, 2, 3), opts_double(trim = 1, fill = "+"))
    construct(c(1, 2, 3), opts_atomic(trim = 0), opts_double(trim = 1, fill = "+"))

    construct(c(1i, 2i, 3i), opts_complex(trim = 1, fill = "+"))
    construct(c(1i, 2i, 3i), opts_double(trim = 0, fill = "+"))
    construct(c(1i, 2i, 3i), opts_atomic(trim = 0), opts_complex(trim = 1, fill = "+"))

    construct(as.raw(c(1,2,3)), opts_raw(trim = 1, fill = "+"))
    construct(as.raw(c(1,2,3)), opts_integer(trim = 0))
    construct(as.raw(c(1,2,3)), opts_double(trim = 0), opts_raw(representation = "decimal"))
    construct(as.raw(c(1,2,3)), opts_atomic(trim = 0), opts_raw(trim = 1, fill = "+"))

    construct(letters, opts_character(trim = 1, fill = "+"))
    construct(letters, opts_atomic(trim = 0), opts_character(trim = 1, fill = "+"))

    construct("🐶", unicode_representation = "ascii")
    construct("🐶", unicode_representation = "ascii", opts_character(unicode_representation = "unicode"))
    construct("🐶", unicode_representation = "unicode", opts_character(unicode_representation = "ascii"))
    construct("🐶", unicode_representation = "ascii", opts_atomic(unicode_representation = "unicode"))
    construct("🐶", unicode_representation = "unicode", opts_atomic(unicode_representation = "ascii"))
    construct("🐶", opts_atomic(unicode_representation = "ascii"), opts_character(unicode_representation = "unicode"))
    construct("🐶", opts_atomic(unicode_representation = "unicode"), opts_character(unicode_representation = "ascii"))
  })
})
