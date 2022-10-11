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
    construct(as.environment(head(cars,2)))
    # env "prototype" with constructor = "new.env"
    construct(as.environment(head(cars,2)), opts_environment(constructor = "new.env"))
    # but only if can't be guessed
    construct(environment(setNames), opts_environment(constructor = "new.env"))
    # envs with a class are correctly forwarded to env method
    env <- new.env()
    class(env) <- "foo"
    construct(env)
    e1 <- new.env(parent = .GlobalEnv)
    e1$x <- 1
    e2 <- new.env(parent = e1)
    e2$y <- 2
    construct(e2) # constructor = "list2env", recurse = FALSE
    construct(e2, opts_environment(recurse = TRUE))
    construct(e2, opts_environment(constructor = "new_environment"))
    construct(e2, opts_environment(constructor = "new_environment", recurse = TRUE))
    construct(e2, opts_environment(constructor = "new.env"))
    construct(e2, opts_environment(constructor = "topenv"))
    construct(e2, opts_environment(constructor = "as.environment"))
  })
})
