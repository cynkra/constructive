# raw

    Code
      construct(raw(1))
    Output
      as.raw(0x00)
    Code
      construct(raw(2))
    Output
      as.raw(c(0x00, 0x00))
    Code
      construct(raw(10))
    Output
      raw(10)
    Code
      construct(structure(raw(2), foo = 1))
    Output
      as.raw(c(0x00, 0x00)) |>
        structure(foo = 1)
    Code
      construct(raw(1), opts_raw(representation = "decimal"))
    Output
      as.raw(0)
    Code
      construct(raw(2), opts_raw(representation = "decimal"))
    Output
      as.raw(c(0, 0))
    Code
      construct(raw(10), opts_raw(representation = "decimal"))
    Output
      raw(10)
    Code
      construct(as.raw(c(104, 101, 108, 108, 111, 119, 111, 114, 108, 100)), opts_raw(
        "charToRaw"))
    Output
      charToRaw("helloworld")

