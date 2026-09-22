# stepfun

    Code
      construct(stepfun(c(1, 2, 3), c(0, 5, 1, 3)))
    Output
      stepfun(c(1, 2, 3), c(0, 5, 1, 3))
    Code
      construct(stepfun(1:3, c(0L, 1L, 2L, 3L)))
    Output
      stepfun(c(1, 2, 3), 0:3) |>
        structure(call = quote(stepfun(1:3, c(0L, 1L, 2L, 3L))))
    Code
      construct(stepfun(c(1, 2, 3), c(0, 5, 1, 3), right = TRUE))
    Output
      stepfun(c(1, 2, 3), c(0, 5, 1, 3), right = TRUE)
    Code
      construct(stepfun(c(1, 2, 3), c(0, 5, 1, 3), f = 0.5))
    Output
      stepfun(c(1, 2, 3), c(0, 5, 1, 3), f = 0.5)
    Code
      construct(stepfun(1, c(TRUE, FALSE)))
    Output
      stepfun(1, c(TRUE, FALSE))
    Code
      construct(structure(stepfun(1, c(0, 1)), call = NULL))
    Output
      stepfun(1, c(0, 1)) |>
        structure(call = NULL)
    Code
      construct(stepfun(c(1, 2, 3), c(0, 5, 1, 3)), opts_stepfun("next"),
      opts_environment("list2env"))
    Output
      (function(v) .approxfun(x, y, v, method, yleft, yright, f, na.rm)) |>
        (`environment<-`)(
          list2env(
            list(
              f = 0,
              method = 2L,
              na.rm = TRUE,
              x = c(1, 2, 3),
              y = c(5, 1, 3),
              yleft = 0,
              yright = 3
            ),
            parent = asNamespace("stats")
          )
        ) |>
        structure(
          class = c("stepfun", "function"),
          call = quote(stepfun(c(1, 2, 3), c(0, 5, 1, 3)))
        )

