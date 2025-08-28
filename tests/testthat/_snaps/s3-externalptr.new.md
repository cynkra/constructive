# externalptr

    Code
      dt <- data.table::data.table(a = 1)
      class(dt) <- "data.frame"
      construct(dt)
    Message
      ! The code built by {constructive} could not be evaluated.
      ! Due to error: "external_pointer" not available for .Call() for package "constructive"
    Output
      data.frame(a = 1) |>
        structure(.internal.selfref = constructive::.xptr("0x123456789"))
    Code
      classed_ptr <- structure(attr(dt, ".internal.selfref"), class = "foo")
      construct(classed_ptr)
    Message
      ! The code built by {constructive} could not be evaluated.
      ! Due to error: "external_pointer" not available for .Call() for package "constructive"
    Output
      constructive::.xptr("0x123456789") |>
        structure(class = "foo")

