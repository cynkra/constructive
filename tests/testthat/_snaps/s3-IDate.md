# IDate

    Code
      construct(data.table::as.IDate("2024-01-01"))
    Output
      data.table::as.IDate("2024-01-01")
    Code
      construct(stats::setNames(data.table::as.IDate(c("2024-01-01", NA, "1900-12-31")),
      c("a", "b", "c")))
    Output
      data.table::as.IDate(c("2024-01-01", NA, "1900-12-31")) |>
        structure(names = c("a", "b", "c"))
    Code
      construct(data.table::as.IDate(NA))
    Output
      data.table::as.IDate(NA_character_)
    Code
      construct(data.table::as.IDate(character()))
    Output
      data.table::as.IDate(character(0))
    Code
      construct(data.table::as.IDate(c(-1000000L, 1L)))
    Output
      data.table::as.IDate(c(-1000000L, 1L))
    Code
      construct(data.table::as.IDate("2024-01-01"), opts_IDate("next"))
    Output
      19723L |>
        structure(class = c("IDate", "Date"))
    Code
      construct(data.table::as.IDate("2024-01-01"), opts_IDate("integer"))
    Output
      19723L |>
        structure(class = c("IDate", "Date"))

