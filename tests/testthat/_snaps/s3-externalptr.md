# externalptr

    Code
      dt <- data.table::data.table(a = 1)
      class(dt) <- "data.frame"
      construct(dt)
    Message
      {constructive} couldn't create code that reproduces perfectly the input
      i Call `construct_issues()` to inspect the last issues
    Output
      data.frame(a = 1) |>
        structure(.internal.selfref = constructive::.xptr("0x123456789"))
    Code
      classed_ptr <- structure(attr(dt, ".internal.selfref"), class = "foo")
      construct(classed_ptr)
    Message
      {constructive} couldn't create code that reproduces perfectly the input
      i Call `construct_issues()` to inspect the last issues
    Output
      constructive::.xptr("0x123456789") |>
        structure(class = "foo")

