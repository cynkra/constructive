# externalptr

    Code
      dt <- data.table::data.table(a = 1)
      class(dt) <- "data.frame"
      construct(dt, check = FALSE)
    Output
      data.frame(a = 1) |>
        structure(.internal.selfref = constructive::.xptr("0x123456789"))
    Code
      classed_ptr <- structure(attr(dt, ".internal.selfref"), class = "foo")
      construct(classed_ptr, check = FALSE)
    Output
      constructive::.xptr("0x123456789") |>
        structure(class = "foo")

