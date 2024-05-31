# dots

    Code
      dots1 <- local((function(...) get("..."))(a = x, y))
      construct(dots1, opts_environment("list2env"))
    Output
      evalq(
        (function(...) get("..."))(a = x, y),
        envir = new.env(parent = asNamespace("constructive"))
      )
    Code
      construct(structure(dots1, class = "foo"), opts_environment("list2env"))
    Output
      evalq(
        (function(...) get("..."))(a = x, y),
        envir = new.env(parent = asNamespace("constructive"))
      ) |>
        structure(class = "foo")
    Code
      f <- (function(...) {
        y <- 1
        g(y = y, ...)
      })
      g <- (function(...) get("..."))
      x <- 1
      dots2 <- local(f(x = x))
      construct(dots2, opts_environment("list2env"))
    Output
      rlang::inject((function(...) get("..."))(!!!list(
        y = rlang::new_quosure(
          quote(y),
          list2env(
            list(
              ... = evalq(
                (function(...) get("..."))(x = x),
                envir = new.env(parent = asNamespace("constructive"))
              ),
              y = 1
            ),
            parent = asNamespace("constructive")
          )
        ),
        x = rlang::new_quosure(quote(x), new.env(parent = asNamespace("constructive")))
      )))

