# language

    Code
      construct(quote(a_symbol))
    Output
      quote(a_symbol)
    Code
      construct(quote(a + call))
    Output
      quote(a + call)
    Code
      construct(body(ave))
    Output
      quote({
        if (missing(...)) x[] <- FUN(x) else {
          g <- interaction(...)
          split(x, g) <- lapply(split(x, g), FUN)
        }
        x
      })
    Code
      construct(quote(expr = ))
    Output
      quote(expr = )

# language after 4.1

    Code
      construct(quote(`🐶`))
    Output
      quote(`\xf0\x9f\x90\xb6`)
    Code
      construct(quote(`🐶`), unicode_representation = "unicode")
    Output
      quote(`🐶`)

# complex language

    Code
      x <- quote(a(1)(2))
      attr(x[[1]], "foo") <- "bar"
      construct(x)
    Output
      as.call(list(
        quote(a(1)) |>
          structure(foo = "bar"),
        2
      ))
    Code
      y <- quote(a(1))
      y[[1]] <- c("a", "vector")
      construct(y)
    Output
      as.call(list(c("a", "vector"), 1))

