# multiplication works

    Code
      obj <- attributes(data.table::data.table(a = 1))
      construct(obj, check = FALSE)
    Condition
      Warning:
      {constructive} cannot reconstruct pointers, <pointer: 0x14f80cee0> was replaced by `NULL`
    Output
      list(
        names = "a",
        row.names = 1L,
        class = c("data.table", "data.frame"),
        .internal.selfref = NULL
      )

