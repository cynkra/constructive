# roman

    Code
      construct(as.roman(c(1, 12, NA, 3999)))
    Output
      as.roman(c("I", "XII", NA, "MMMCMXCIX"))
    Code
      construct(as.roman(integer()))
    Output
      as.roman(character(0))
    Code
      construct(structure(as.roman(c(1, 12)), names = c("a", "b")))
    Output
      as.roman(c("I", "XII")) |>
        structure(names = c("a", "b"))
    Code
      construct(structure(c(1L, 4000L), class = "roman"))
    Output
      c(1L, 4000L) |>
        structure(class = "roman")
    Code
      construct(as.roman(c(1, 12, NA)), opts_roman("next"))
    Output
      c(1L, 12L, NA) |>
        structure(class = "roman")

