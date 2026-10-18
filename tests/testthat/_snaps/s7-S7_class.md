# S7_class

    Code
      construct(S7::S7_object)
    Output
      S7::S7_object

# S7_class, next, S7 <= 0.2.1

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

# S7_class, next, S7 > 0.2.1

    Code
      construct(S7::S7_object, opts_S7_class("next"), check = FALSE)
    Output
      (function() {
        .Call(S7_object_)
      }) |>
        (`environment<-`)(constructive::.env("0x123456789", parents = "namespace:S7")) |>
        structure(
          name = "S7_object",
          properties = list(),
          abstract = FALSE,
          constructor = (function() {
            .Call(S7_object_)
          }) |>
            (`environment<-`)(constructive::.env("0x123456789", parents = "namespace:S7")),
          validator = (function(self) {
            if (!is_S7_type(self)) {
              "Underlying data is corrupt"
            }
          }) |>
            (`environment<-`)(asNamespace("S7")),
          S7_class_name = "S7_object",
          S7_dispatch = "S7_object",
          S7_version = 1L,
          class = c("S7_class", "S7_object")
        )

