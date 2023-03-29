# externalptr

    Code
      dt <- data.table::data.table(a = 1)
      class(dt) <- "data.frame"
      construct(dt)
    Output
      data.frame(a = 1) |>
        structure(.internal.selfref = constructive::external_pointer("0x12a80dee0"))

