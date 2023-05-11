# Encoding

    Code
      construct(data.frame(x = c("ü", "a"), y = c("long_enough_for_multiline_output")))
    Output
      data.frame(
        x = c("\U{FC}", "a"),
        y = c("long_enough_for_multiline_output", "long_enough_for_multiline_output")
      )

