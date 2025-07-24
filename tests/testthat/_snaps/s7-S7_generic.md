# S7_generic

    Code
      type_of <- S7::new_generic("type_of", dispatch_args = "x")
      construct(type_of)
    Output
      S7::new_generic("type_of", dispatch_args = "x")
    Code
      environment(attr(type_of, "S7_class")) <- .GlobalEnv
      environment(attr(attr(type_of, "S7_class"), "constructor")) <- .GlobalEnv
      construct(type_of, opts_S7_generic("next"), opts_function(srcref = TRUE))
    Message
      ! The code built by {constructive} could not be evaluated.
      ! Due to error: No environment was found at the memory address '0x123456789'
      i It's likely that {constructive} was called in a different session to generate this code.
      i The environment might also have been garbage collected.
      i See `?opts_environment` for various alternatives to construct environment with persistent definitions.
    Output
      (function(x, ...) S7::S7_dispatch()) |>
        (`environment<-`)(
          constructive::.env(
            "0x123456789",
            parents = c("0x123456789", "0x123456789", "namespace:constructive")
          )
        ) |>
        structure(
          class = c("S7_generic", "function", "S7_object"),
          S7_class = (S7::new_class(
            "S7_generic",
            parent = S7::class_function,
            properties = list(
              name = S7::class_character,
              methods = S7::class_environment,
              dispatch_args = S7::class_character
            ),
            constructor = as.function(
              list(
                .data = quote(function() NULL),
                name = character(0),
                methods = quote(new.env(parent = emptyenv())),
                dispatch_args = character(0),
                quote(new_object(fun(.data = .data), name = name, methods = methods, dispatch_args = dispatch_args))
              ),
              envir = .GlobalEnv
            )
          )) |>
            (`environment<-`)(.GlobalEnv),
          name = "type_of",
          methods = constructive::.env("0x123456789", parents = "empty"),
          dispatch_args = "x",
          srcref = NULL
        )
    Code
      S7::method(type_of, S7::class_character) <- (function(x, ...)
        "A character vector")
      S7::method(type_of, S7::new_S3_class("data.frame")) <- (function(x, ...)
        "A data frame")
      S7::method(type_of, S7::class_function) <- (function(x, ...) "A function")
      construct(type_of)
    Message
      ! The code built by {constructive} could not be evaluated.
      ! Due to error: No environment was found at the memory address '0x123456789'
      i It's likely that {constructive} was called in a different session to generate this code.
      i The environment might also have been garbage collected.
      i See `?opts_environment` for various alternatives to construct environment with persistent definitions.
    Output
      S7::new_generic("type_of", dispatch_args = "x") |>
        (S7::`method<-`)(
          S7::new_S3_class("data.frame"),
          value = (function(x, ...) "A data frame") |>
            (`environment<-`)(
              constructive::.env(
                "0x123456789",
                parents = c("0x123456789", "0x123456789", "namespace:constructive")
              )
            )
        ) |>
        (S7::`method<-`)(
          S7::class_character,
          value = (function(x, ...) "A character vector") |>
            (`environment<-`)(
              constructive::.env(
                "0x123456789",
                parents = c("0x123456789", "0x123456789", "namespace:constructive")
              )
            )
        ) |>
        (S7::`method<-`)(
          S7::class_function,
          value = (function(x, ...) "A function") |>
            (`environment<-`)(
              constructive::.env(
                "0x123456789",
                parents = c("0x123456789", "0x123456789", "namespace:constructive")
              )
            )
        )

