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
  expect_pipe_snapshot({
    construct(evalq(x ~ y, asNamespace("stats")))
    construct(evalq(x ~ y, asNamespace("stats")), opts_formula(environment = FALSE))
    construct(evalq(x ~ y, asNamespace("stats")), opts_formula(environment = FALSE), compare = compare_options(ignore_formula_env = TRUE))
  })
})
