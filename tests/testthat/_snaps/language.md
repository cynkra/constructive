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

