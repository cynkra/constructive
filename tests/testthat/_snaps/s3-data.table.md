# data.table

    Code
      dt1 <- data.table::data.table(head(cars, 2))
      construct(dt1)
    Output
      data.table::data.table(speed = c(4, 4), dist = c(2, 10))
    Code
      construct(dt1, opts_data.table(selfref = TRUE))
    Output
      data.table::data.table(speed = c(4, 4), dist = c(2, 10)) |>
        structure(.internal.selfref = constructive::.xptr("0x123456789"))
    Code
      construct(dt1, opts_data.table("next"))
    Output
      data.frame(speed = c(4, 4), dist = c(2, 10)) |>
        structure(
          class = c("data.table", "data.frame"),
          .internal.selfref = constructive::.xptr("0x123456789")
        )
    Code
      construct(dt1, opts_data.table("list"))
    Output
      list(speed = c(4, 4), dist = c(2, 10)) |>
        structure(
          row.names = 1:2,
          class = c("data.table", "data.frame"),
          .internal.selfref = constructive::.xptr("0x123456789")
        )
    Code
      dt2 <- data.table::data.table(dt1, key = "speed")
      construct(dt2)
    Output
      data.table::data.table(speed = c(4, 4), dist = c(2, 10), key = "speed")

