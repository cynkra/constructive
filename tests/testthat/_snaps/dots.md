# dots

    Code
      dots1 <- local((function(...) environment()$...)(a = x, y))
      construct(dots1)
    Output
      evalq(
        (function(...) environment()$...)(a = x, y),
        envir = new.env(parent = asNamespace("constructive"))
      )
    Code
      f <- (function(...) {
        y <- 1
        g(y = y, ...)
      })
      g <- (function(...) environment()$...)
      x <- 1
      dots2 <- local(f(x = x))
      construct(dots2)
    Output
      rlang::inject((function(...) environment()$...)(!!!list(
        y = rlang::as_quosure(~y, list2env(list(y = 1), parent = asNamespace("constructive"))),
        x = rlang::as_quosure(~x, new.env(parent = asNamespace("constructive")))
      )))

