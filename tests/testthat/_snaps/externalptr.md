# externalptr

    Code
      suppressWarnings({
        obj <- attributes(data.table::data.table(a = 1))
        construct(obj, check = FALSE)
      })
    Output
      list(
        names = "a",
        row.names = 1L,
        class = c("data.table", "data.frame"),
        .internal.selfref = NULL
      )

