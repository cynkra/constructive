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
    construct(as.environment(head(cars,2)), check = FALSE)
    # env "prototype" with `env_as_list = FALSE`
    construct(as.environment(head(cars,2)), check = FALSE, env_as_list = FALSE)
    # but only if can't be guessed
    construct(environment(setNames), env_as_list = FALSE)
    # envs with a class are correctly forwarded to env method
    env <- new.env()
    class(env) <- "foo"
    construct(env, check = FALSE)
  })
})
