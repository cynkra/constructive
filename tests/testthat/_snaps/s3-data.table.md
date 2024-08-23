# data.table

    Code
      dt1 <- data.table::data.table(head(cars, 2))
      construct(dt1)
    Output
      data.table::data.table(speed = 4, dist = c(2, 10))
    Code
      construct(dt1, opts_data.table(selfref = TRUE))
    Output
      data.table::data.table(speed = 4, dist = c(2, 10)) |>
        structure(.internal.selfref = constructive::.xptr("0x123456789"))
    Code
      construct(dt1, opts_data.table("next"))
    Output
      data.frame(speed = 4, dist = c(2, 10)) |>
        structure(
          class = c("data.table", "data.frame"),
          .internal.selfref = constructive::.xptr("0x123456789")
        )
    Code
      construct(dt1, opts_data.table("list"))
    Output
      list(speed = c(4, 4), dist = c(2, 10)) |>
        structure(
          row.names = c(NA, -2L),
          class = c("data.table", "data.frame"),
          .internal.selfref = constructive::.xptr("0x123456789")
        )
    Code
      dt2 <- data.table::data.table(dt1, key = "speed")
      construct(dt2)
    Output
      data.table::data.table(speed = 4, dist = c(2, 10), key = "speed")

# recycle in data tables

    Code
      construct(data.table::data.table(a = 1:2, b = c(1, 1)))
    Output
      data.table::data.table(a = 1:2, b = 1)
    Code
      construct(data.table::data.table(a = c(1, 1), b = c(1, 1)))
    Output
      data.table::data.table(a = c(1, 1), b = 1)
    Code
      construct(data.table::data.table(a = 1:2, b = factor(c("a", "a"))))
    Output
      data.table::data.table(a = 1:2, b = factor("a"))
    Code
      construct(data.table::data.table(a = 1:2, b = as.Date(c("2000-01-01",
        "2000-01-01"))))
    Output
      data.table::data.table(a = 1:2, b = as.Date("2000-01-01"))

# duplicate names in data tables

    Code
      construct(data.table::data.table(a = 1, a = 2))
    Output
      data.table::data.table(a = 1, a = 2)

