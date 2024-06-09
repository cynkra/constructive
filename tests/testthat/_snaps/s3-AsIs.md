# AsIs

    Code
      construct(I(month.abb))
    Output
      I(c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"))
    Code
      construct(I(month.abb), opts_AsIs("next"))
    Output
      c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec") |>
        structure(class = "AsIs")
    Code
      construct(I(head(cars, 2)))
    Output
      I(data.frame(speed = c(4, 4), dist = c(2, 10)))
    Code
      x <- 1
      class(x) <- c("AsIs", "foo")
      construct(x)
    Output
      I(1 |>
        structure(class = "foo"))
    Code
      class(x) <- c("foo", "AsIs")
      construct(x)
    Output
      1 |>
        structure(class = c("foo", "AsIs"))

