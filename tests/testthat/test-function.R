test_that("function", {
  expect_snapshot({
    f1 <- as.function(alist(x=, x), .GlobalEnv)
    f2 <- as.function(alist(x=, {x}), .GlobalEnv)

    construct(f1)
    construct(f2)

    construct(f1, opts_function(environment = TRUE))
    construct(f1, opts_function(srcref = TRUE))
    construct(f2, opts_function(srcref = TRUE))

    construct(f1, opts_function("as.function"))
    construct(f2, opts_function("as.function"))
    construct(f1, opts_function("as.function", environment = FALSE))

    construct(f1, opts_function("new_function"))
    construct(f2, opts_function("new_function"))
    construct(f1, opts_function("new_function", environment = FALSE))

    construct(setNames, opts_function(environment = TRUE))
    construct(setNames, opts_function("as.function", environment = TRUE))
    # with trim
    construct(setNames, opts_function(trim = 1))

    # primitives
    construct(`+`)

    # functions with a class
    f4 <- f1
    class(f4) <- "foo"
    construct(f4)
  })
})

