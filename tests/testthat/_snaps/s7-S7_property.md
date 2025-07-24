# S7_property

    Code
      slices <- S7::new_property(S7::class_numeric, default = 10)
      construct(slices)
    Output
      S7::new_property(S7::class_numeric, default = 10)
    Code
      construct(slices, opts_S7_property("next"))
    Output
      list(
        name = NULL,
        class = S7::class_numeric,
        getter = NULL,
        setter = NULL,
        validator = NULL,
        default = 10
      ) |>
        structure(class = "S7_property")
    Code
      now <- S7::new_property(getter = function(self) Sys.time())
      environment(now$getter) <- .GlobalEnv
      construct(now)
    Output
      S7::new_property(
        getter = (function(self) Sys.time()) |>
          (`environment<-`)(.GlobalEnv)
      )
    Code
      construct(now, opts_S7_property("next"))
    Output
      list(
        name = NULL,
        class = S7::class_any,
        getter = (function(self) Sys.time()) |>
          (`environment<-`)(.GlobalEnv),
        setter = NULL,
        validator = NULL,
        default = NULL
      ) |>
        structure(class = "S7_property")

