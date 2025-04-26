# language

    Code
      construct(quote(a_symbol))
    Output
      quote(a_symbol)
    Code
      construct(as.symbol("a\\b"))
    Output
      quote(`a\\b`)
    Code
      construct(quote(a + call))
    Output
      quote(a + call)
    Code
      construct(quote(expr = ))
    Output
      quote(expr = )
    Code
      construct(as.call(list(quote(expr = ))))
    Output
      as.call(list(quote(expr = )))

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

# We can construct calls with empty callers

    Code
      construct(substitute(X(), list(X = quote(expr = ))))
    Output
      as.call(list(quote(expr = )))
    Code
      construct(substitute({
        X(1, 2)
      }, list(X = quote(expr = ))))
    Output
      as.call(list(quote(`{`), as.call(list(quote(expr = ), 1, 2))))

# We can construct calls with non syntactic literals

    Code
      construct(call("fun", -1))
    Output
      as.call(list(quote(fun), -1))
    Code
      construct(call("fun", 1 + 0+0i))
    Output
      as.call(list(quote(fun), 1+0i))
    Code
      construct(call("fun", quote(expr = )))
    Output
      as.call(list(quote(fun), quote(expr = )))
    Code
      construct(call("+", quote(expr = )))
    Output
      as.call(list(quote(`+`), quote(expr = )))

