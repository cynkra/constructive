test_that("environment", {
  expect_snapshot({
    # handle special cases
    construct(globalenv())
    construct(baseenv())
    construct(as.environment("package:base"))
    construct(asNamespace("base"))
    construct(as.environment("Autoloads"))
    construct(environment(setNames))
    # env from list
    construct(as.environment(head(cars,2)), opts_environment("list2env"))
    # env "prototype" with constructor = "new.env"
    construct(as.environment(head(cars,2)), opts_environment(constructor = "new.env"))
    # but only if can't be guessed
    construct(environment(setNames), opts_environment(constructor = "new.env"))
    # envs with a class are correctly forwarded to env method
    env <- new.env(parent = asNamespace("stats"))
    class(env) <- "foo"
    construct(env, opts_environment("list2env"))
    e1 <- new.env(parent = .GlobalEnv)
    e1$x <- 1
    e2 <- new.env(parent = e1)
    e2$y <- 2
    e2$.z <- 3
    construct(e2, opts_environment(constructor = "list2env")) # constructor = "list2env", recurse = FALSE
    construct(e2, opts_environment(constructor = "new_environment"))
    construct(e2, opts_environment(constructor = "new.env"))
    construct(e2, opts_environment(constructor = "topenv"))
    construct(e2, opts_environment(constructor = "as.environment"))
    # circularity
    evalq({
    e <- new.env()
    e$f <- e
    foo <- evalq(~a, e)
    construct(foo, opts_environment("predefine"), opts_formula(environment = TRUE))
    }, .GlobalEnv)
  })

  # FIXME: fails on CI, because of segfault, why ?
  # expect_error(constructive::.env("0x123456789"), "No environment was found")

  skip_if(with_versions(R < "4.2"))
  expect_snapshot({
    construct(e2, opts_environment(constructor = "list2env", recurse = TRUE))
    construct(e2, opts_environment(constructor = "new_environment", recurse = TRUE))
  })

  skip_if(identical(Sys.getenv("R_COVR"), "true"))
  expect_snapshot({
    construct(constructive::.cstr_construct, opts_environment("predefine"), opts_function(environment = TRUE))
  })

  expect_snapshot({
    e <- rlang::env(.GlobalEnv, a = 1, b = 2, c = 3, d = 4)
    construct(e, check = FALSE)
    lockEnvironment(e)
    construct(e, check = FALSE)
    construct(e, opts_environment("list2env"))
    lockBinding("a", e)
    construct(e, opts_environment("list2env"))
    lockBinding("b", e)
    construct(e, opts_environment("list2env"))
    lockBinding("c", e)
    construct(e, opts_environment("list2env"))
    lockBinding("d", e)
    construct(e, opts_environment("list2env"))
  })

  expect_snapshot({
    construct(getNamespaceInfo("datasets", "lazydata"))
    construct(parent.env(asNamespace("stats")))
  })
})

test_that("environments with names method are constructed properly", {
  env <- new.env()
  env$x <- 1
  class(env) <- "foo"
  names.foo <- function(x) "y"
  expect_snapshot({
    construct(env, opts_environment("list2env"), check = FALSE)
  })
})



