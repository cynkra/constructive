# ecdf

    Code
      construct(ecdf(c(1, 2, 2, 3)))
    Output
      ecdf(c(1, 2, 2, 3))
    Code
      x <- c(3, 1, NA, 2.5, 3)
      construct(ecdf(x))
    Output
      ecdf(c(1, 2.5, 3, 3)) |>
        structure(call = quote(ecdf(x)))
    Code
      construct(ecdf(1:3))
    Output
      ecdf(c(1, 2, 3)) |>
        structure(call = quote(ecdf(1:3)))
    Code
      construct(ecdf(c(1, 2, 2, 3)), opts_ecdf("next"), opts_environment("list2env"))
    Output
      (function(v) .approxfun(x, y, v, method, yleft, yright, f, na.rm)) |>
        (`environment<-`)(
          list2env(
            list(
              f = 0,
              method = 2L,
              na.rm = TRUE,
              nobs = 4L,
              x = c(1, 2, 3),
              y = c(0.25, 0.75, 1),
              yleft = 0,
              yright = 1
            ),
            parent = asNamespace("stats")
          )
        ) |>
        structure(class = c("ecdf", "stepfun", "function"), call = quote(ecdf(c(1, 2, 2, 3))))
    Code
      e <- ecdf(c(1, 2, 2, 3))
      environment(e) <- list2env(list(f = 0, method = 2L, na.rm = TRUE, nobs = 4L, x = c(
        1, 2), y = c(0.3, 1), yleft = 0, yright = 1), parent = asNamespace("stats"))
      construct(e, opts_environment("list2env"))
    Output
      (function(v) .approxfun(x, y, v, method, yleft, yright, f, na.rm)) |>
        (`environment<-`)(
          list2env(
            list(
              f = 0,
              method = 2L,
              na.rm = TRUE,
              nobs = 4L,
              x = c(1, 2),
              y = c(0.3, 1),
              yleft = 0,
              yright = 1
            ),
            parent = asNamespace("stats")
          )
        ) |>
        structure(class = c("ecdf", "stepfun", "function"), call = quote(ecdf(c(1, 2, 2, 3))))

