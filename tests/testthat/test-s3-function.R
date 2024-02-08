test_that("function", {
  expect_pipe_snapshot({
    f1 <- as.function(alist(x=, x), .GlobalEnv)
    f2 <- as.function(alist(x=, {x}), .GlobalEnv)

    construct(f1)
    construct(f2)
    construct(f1, opts_function(environment = FALSE))

    construct(f1, opts_function(srcref = TRUE, environment = FALSE))
    construct(f2, opts_function(srcref = TRUE, environment = FALSE))

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

    # use srcref to keep comments
    # testthat seems to remove srcrefs so we build it artificially
    f5 <- (function(x) {
      x
    }) %>%
      structure(
        srcref = c(1L, 8L, 4L, 1L, 8L, 1L, 1L, 4L) %>%
          structure(
            srcfile = list2env(
              list(
                fixedNewlines = TRUE,
                lines = c("foo <- function(x) {", "  # foo", "  x", "}", ""),
                filename = ""
              ),
              parent = .GlobalEnv
            ) %>%
              structure(class = c("srcfilecopy", "srcfile")),
            class = "srcref"
          )
      )
    construct(f5, opts_function(environment = FALSE), pipe = "magrittr")

    # function without body and without srcref
    f6 <- function() NULL
    attr(f6, "srcref") <- NULL
    construct(f6, opts_function(environment = FALSE))

    f7 <- f2
    body(f7) <- structure(body(f7), some_attr = "hello")
    construct(f7, opts_function(environment = FALSE))

  })
})

