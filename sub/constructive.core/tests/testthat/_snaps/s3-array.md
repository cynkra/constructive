# array

    Code
      construct(as.array(month.abb))
    Output
      array(
        c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"),
        dim = 12L
      )
    Code
      construct(as.array(month.abb), opts_array("next"))
    Output
      c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec") |>
        structure(dim = 12L)
    Code
      construct(array(1:3, c(2, 4)))
    Output
      matrix(c(1L, 2L, 3L, 1L, 2L, 3L, 1L, 2L), nrow = 2L, ncol = 4L)
    Code
      construct(structure(1, class = "array"))
    Output
      1 |>
        structure(class = "array")
    Code
      construct(structure(1, class = "array", dim = 1))
    Output
      array(1, dim = 1L) |>
        structure(class = "array")

# classed array

    Code
      construct(structure(array(1:27, c(3, 3, 3)), class = "a"))
    Output
      array(1:27, dim = rep(3L, 3L)) |>
        structure(class = "a")

