test_that("The data arg works", {
  expect_snapshot({
    construct(list(letters), data = "base") # the base env is a bit different
    construct(list(iris), data = "datasets")
    construct(list(letters), data = baseenv())
    construct(list(letters), data = list(foo = letters))
    # if two choices, pick first
    construct(list(letters), data = list(foo = letters, bar = letters))
    # use namespace notation only if ambiguous
    construct(list(data.table::first, dplyr::first, dplyr::select), data = list("dplyr", "data.table"))
  })

  expect_error(
    construct(1, data = list(foo = 2, foo = 3)),
    "duplicate"
  )
})

test_that("noquote is supported", {
  expect_snapshot({
    construct(noquote("a"))
    construct(noquote(list("a", "b")))
  })
})

test_that("compare_options", {
  expect_snapshot({
    construct(evalq(x ~ y, asNamespace("stats")))
    construct(evalq(x ~ y, asNamespace("stats")), opts_formula(environment = FALSE))
    construct(evalq(x ~ y, asNamespace("stats")), opts_formula(environment = FALSE), compare = compare_options(ignore_formula_env = TRUE))
  })
})

test_that("backslash and emojis in names work", {
  expect_snapshot({
    construct(c("\\" = "\\"))
  })
})


test_that("backslash and emojis in names work for R >= 4.1", {
  # Due to bypass.R
  skip_if(base::`<`(getRversion(), "4.1"))

  expect_snapshot({
    construct(c("\\🐶" = "\\"), unicode_representation = "unicode")
    construct(c("\\🐶" = "\\"))
  })
})

test_that("the lengths() shim matches base::lengths() without dispatching", {
  x <- list(a = 1:2, b = NULL)
  expect_identical(constructive:::lengths(x), c(a = 2L, b = 0L))
  expect_identical(constructive:::lengths(x, use.names = FALSE), c(2L, 0L))
  expect_identical(constructive:::lengths(list()), integer(0))
  expect_identical(constructive:::lengths(c("a", "b")), c(1L, 1L))

  lying <- structure(list(1:3, 1), class = "lying")
  local_bindings(
    length.lying = function(x) 99L,
    as.list.lying = function(x, ...) stop("dispatched"),
    .env = globalenv()
  )
  expect_identical(base::lengths(list(lying)), 99L)
  expect_identical(constructive:::lengths(list(lying)), 2L)
  expect_identical(constructive:::lengths(lying), c(3L, 1L))
})
