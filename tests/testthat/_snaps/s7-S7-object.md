# S7_object

    Code
      construct(S7::S7_object())
    Output
      S7::S7_object()

# S7_object, S7 <= 0.2.2

    Code
      construct(S7::S7_object(), opts_S7_object("next"))
    Output
      getClass("S4")@prototype |>
        asS3(complete = FALSE) |>
        structure(class = "S7_object")

# S7_object, S7 > 0.2.2

    Code
      construct(S7::S7_object(), opts_S7_object("next"))
    Output
      getClass("S4")@prototype |>
        asS3(complete = FALSE) |>
        structure(class = "S7_object", `_S7_version` = 1L)

