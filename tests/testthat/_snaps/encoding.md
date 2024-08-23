# Encoding

    Code
      construct(data.frame(x = c("ü", "a"), y = c(
        "loooooooooooooooooooooooooooooooooong_enough_for_multiline_output")))
    Output
      data.frame(
        x = c("\U{FC}", "a"),
        y = "loooooooooooooooooooooooooooooooooong_enough_for_multiline_output"
      )

