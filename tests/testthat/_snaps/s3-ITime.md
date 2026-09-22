# ITime

    Code
      construct(data.table::as.ITime("12:00:00"))
    Output
      data.table::as.ITime("12:00:00")
    Code
      construct(stats::setNames(data.table::as.ITime(c("00:00:00", NA, "23:59:59")),
      c("a", "b", "c")))
    Output
      data.table::as.ITime(c("00:00:00", NA, "23:59:59")) |>
        structure(names = c("a", "b", "c"))
    Code
      construct(data.table::as.ITime(NA))
    Output
      data.table::as.ITime(NA_character_)
    Code
      construct(data.table::as.ITime(character()))
    Output
      data.table::as.ITime(character(0))
    Code
      construct(structure(c(-5L, 90000L), class = "ITime"))
    Output
      c(-5L, 90000L) |>
        structure(class = "ITime")
    Code
      construct(data.table::as.ITime("12:00:00"), opts_ITime("next"))
    Output
      43200L |>
        structure(class = "ITime")
    Code
      construct(data.table::as.ITime("12:00:00"), opts_ITime("integer"))
    Output
      43200L |>
        structure(class = "ITime")

