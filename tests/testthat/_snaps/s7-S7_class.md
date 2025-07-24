# S7_class

    Code
      construct(S7::S7_object)
    Output
      S7::S7_object
    Code
      construct(S7::S7_object, opts_S7_class("next"))
    Output
      (function() {
        .Call(S7_object_)
      }) |>
        (`environment<-`)(asNamespace("S7")) |>
        structure(
          name = "S7_object",
          properties = list(),
          abstract = FALSE,
          constructor = (function() {
            .Call(S7_object_)
          }) |>
            (`environment<-`)(asNamespace("S7")),
          validator = (function(self) {
            if (!is_S7_type(self)) {
              "Underlying data is corrupt"
            }
          }) |>
            (`environment<-`)(asNamespace("S7")),
          class = c("S7_class", "S7_object")
        )

