test_that("numeric", {
  expect_pipe_snapshot({
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
    construct("ü", opts_atomic(unicode_representation = "latin"))
    construct("ü", check = FALSE)
    construct("ü\\", opts_atomic(unicode_representation = "latin", escape = FALSE), check = FALSE)
    construct("ü\\", opts_atomic(escape = FALSE))
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
    # construct(NA_complex_)
    # construct(c(NA_complex_, NA_complex_))
    # construct(c(NA_complex_, NA_complex_, NA_complex_))
    # construct(c(NA_complex_, NA_complex_, NA_complex_), opts_atomic(compress = FALSE))
    # construct(c(NA_complex_, 1))
    # construct(c(NA_complex_, 1i))
    # construct(1e-10 + 1e10i)
    # construct(c(1e-10 + 1e10i, 2e-10 + 2e10i))
  })
})


test_that("NA and empty names", {
  expect_pipe_snapshot({
    construct(structure(logical(2), names = c("", "")))
    construct(structure(logical(2), names = c("", NA)))
    construct(structure(logical(2), names = c(NA, NA)))
    construct(structure(logical(2), names = c(NA, "a")))
    construct(structure(logical(2), names = c("", "a")))
    construct(structure(logical(2), names = c("", "a")))
    construct(structure(logical(10), names = c("", "a")))

    construct(structure(integer(2), names = c("", "")))
    construct(structure(integer(2), names = c("", NA)))
    construct(structure(integer(2), names = c(NA, NA)))
    construct(structure(integer(2), names = c(NA, "a")))
    construct(structure(integer(2), names = c("", "a")))
    construct(structure(integer(10), names = c("", "a")))

    construct(structure(double(2), names = c("", "")))
    construct(structure(double(2), names = c("", NA)))
    construct(structure(double(2), names = c(NA, NA)))
    construct(structure(double(2), names = c(NA, "a")))
    construct(structure(double(2), names = c("", "a")))
    construct(structure(double(10), names = c("", "a")))

    construct(structure(complex(2), names = c("", "")))
    construct(structure(complex(2), names = c("", NA)))
    construct(structure(complex(2), names = c(NA, NA)))
    construct(structure(complex(2), names = c(NA, "a")))
    construct(structure(complex(2), names = c("", "a")))
    construct(structure(complex(10), names = c("", "a")))

    construct(structure(character(2), names = c("", "")))
    construct(structure(character(2), names = c("", NA)))
    construct(structure(character(2), names = c(NA, NA)))
    construct(structure(character(2), names = c(NA, "a")))
    construct(structure(character(2), names = c("", "a")))
    construct(structure(character(10), names = c("", "a")))

    # construct(structure(raw(2), names = c("", "")))
    # construct(structure(raw(2), names = c("", NA)))
    # construct(structure(raw(2), names = c(NA, NA)))
    # construct(structure(raw(2), names = c(NA, "a")))
    # construct(structure(raw(2), names = c("", "a")))
    # construct(structure(raw(10), names = c("", "a")))
  })
})
