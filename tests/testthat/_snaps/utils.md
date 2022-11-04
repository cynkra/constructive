# pipe works for one liners

    Code
      x <- 1
      attr(x, "foo") <- 2
      construct(x, one_liner = TRUE)
    Output
      1 |> structure(foo = 2)

# data

    Code
      construct(cars, data = "datasets")
    Output
      cars
    Code
      construct(mean, data = "base")
    Output
      mean
    Code
      construct(mean, data = asNamespace("base"))
    Output
      mean
    Code
      construct(list(mean, cars), data = list(asNamespace("base"), "datasets"))
    Output
      list(mean, cars)

