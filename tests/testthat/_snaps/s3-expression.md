# expression vectors

    Code
      x <- expression(a = 1, x + y)
      construct(x)
    Output
      expression(a = 1, x + y)
    Code
      x[[2]] <- structure(quote(x + y), foo = 1)
      construct(x)
    Output
      expression(a = 1, NULL) |>
        (`[[<-`)(
          2L,
          value = quote(x + y) |>
            structure(foo = 1)
        )
    Code
      x[[2]] <- expression(x + y)
      construct(x)
    Output
      expression(a = 1, NULL) |>
        (`[[<-`)(2L, value = expression(x + y))
    Code
      names(x)[[1]] <- NA
      construct(x)
    Output
      expression(1, NULL) |>
        (`[[<-`)(2L, value = expression(x + y)) |>
        structure(names = c(NA, ""))

