# The data arg works

    Code
      construct(list(letters), data = "base")
    Output
      list(letters)
    Code
      construct(list(iris), data = "datasets")
    Output
      list(iris)
    Code
      construct(list(letters), data = baseenv())
    Output
      list(letters)
    Code
      construct(list(letters), data = list(foo = letters))
    Output
      list(foo)
    Code
      construct(list(letters), data = list(foo = letters, bar = letters))
    Output
      list(foo)
    Code
      construct(list(data.table::first, dplyr::first, dplyr::select), data = list(
        "dplyr", "data.table"))
    Output
      list(data.table::first, dplyr::first, select)

# noquote is supported

    Code
      construct(noquote("a"))
    Output
      noquote("a")
    Code
      construct(noquote(list("a", "b")))
    Output
      noquote(list("a", "b"))

# compare_options

    Code
      construct(evalq(x ~ y, asNamespace("stats")))
    Output
      (x ~ y) |>
        structure(.Environment = asNamespace("stats"))
    Code
      construct(evalq(x ~ y, asNamespace("stats")), opts_formula(environment = FALSE))
    Message
      {constructive} couldn't create code that reproduces perfectly the input
      i Call `construct_issues()` to inspect the last issues
    Output
      x ~ y
    Code
      construct(evalq(x ~ y, asNamespace("stats")), opts_formula(environment = FALSE),
      compare = compare_options(ignore_formula_env = TRUE))
    Output
      x ~ y

