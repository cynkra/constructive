# S7_union

    Code
      construct(S7::class_vector)
    Output
      S7::class_vector
    Code
      construct(S7::class_vector, opts_S7_union("|"))
    Output
      S7::class_logical | S7::class_integer | S7::class_double | S7::class_complex | S7::class_character | S7::class_raw | S7::class_expression | S7::class_list
    Code
      construct(S7::class_vector, opts_S7_union("new_union"))
    Output
      S7::new_union(
        S7::class_logical, S7::class_integer, S7::class_double, S7::class_complex,
        S7::class_character, S7::class_raw, S7::class_expression, S7::class_list
      )
    Code
      construct(S7::class_vector, opts_S7_union("next"))
    Output
      list(
        classes = list(
          S7::class_logical, S7::class_integer, S7::class_double, S7::class_complex,
          S7::class_character, S7::class_raw, S7::class_expression, S7::class_list
        )
      ) |>
        structure(class = "S7_union")

# S7_union, S7 <= 0.2.2

    Code
      construct(S7::class_vector, opts_S7_union("next"), opts_S7_base_class("next"),
      check = FALSE)
    Output
      list(
        classes = list(
          list(
            class = "logical",
            constructor_name = "logical",
            constructor = as.function(list(.data = logical(0), quote(.data)), envir = baseenv()),
            validator = (function(object) {
              if (base_class(object) != name) {
                sprintf("Underlying data must be <%s> not <%s>", name, base_class(object))
              }
            }) |>
              (`environment<-`)(constructive::.env("0x123456789", parents = "namespace:S7"))
          ) |>
            structure(class = "S7_base_class"),
          list(
            class = "integer",
            constructor_name = "integer",
            constructor = as.function(list(.data = integer(0), quote(.data)), envir = baseenv()),
            validator = (function(object) {
              if (base_class(object) != name) {
                sprintf("Underlying data must be <%s> not <%s>", name, base_class(object))
              }
            }) |>
              (`environment<-`)(constructive::.env("0x123456789", parents = "namespace:S7"))
          ) |>
            structure(class = "S7_base_class"),
          list(
            class = "double",
            constructor_name = "double",
            constructor = as.function(list(.data = numeric(0), quote(.data)), envir = baseenv()),
            validator = (function(object) {
              if (base_class(object) != name) {
                sprintf("Underlying data must be <%s> not <%s>", name, base_class(object))
              }
            }) |>
              (`environment<-`)(constructive::.env("0x123456789", parents = "namespace:S7"))
          ) |>
            structure(class = "S7_base_class"),
          list(
            class = "complex",
            constructor_name = "complex",
            constructor = as.function(list(.data = complex(0), quote(.data)), envir = baseenv()),
            validator = (function(object) {
              if (base_class(object) != name) {
                sprintf("Underlying data must be <%s> not <%s>", name, base_class(object))
              }
            }) |>
              (`environment<-`)(constructive::.env("0x123456789", parents = "namespace:S7"))
          ) |>
            structure(class = "S7_base_class"),
          list(
            class = "character",
            constructor_name = "character",
            constructor = as.function(list(.data = character(0), quote(.data)), envir = baseenv()),
            validator = (function(object) {
              if (base_class(object) != name) {
                sprintf("Underlying data must be <%s> not <%s>", name, base_class(object))
              }
            }) |>
              (`environment<-`)(constructive::.env("0x123456789", parents = "namespace:S7"))
          ) |>
            structure(class = "S7_base_class"),
          list(
            class = "raw",
            constructor_name = "raw",
            constructor = as.function(list(.data = raw(0), quote(.data)), envir = baseenv()),
            validator = (function(object) {
              if (base_class(object) != name) {
                sprintf("Underlying data must be <%s> not <%s>", name, base_class(object))
              }
            }) |>
              (`environment<-`)(constructive::.env("0x123456789", parents = "namespace:S7"))
          ) |>
            structure(class = "S7_base_class"),
          list(
            class = "expression",
            constructor_name = "expression",
            constructor = as.function(list(.data = expression(), quote(.data)), envir = baseenv()),
            validator = (function(object) {
              if (base_class(object) != name) {
                sprintf("Underlying data must be <%s> not <%s>", name, base_class(object))
              }
            }) |>
              (`environment<-`)(constructive::.env("0x123456789", parents = "namespace:S7"))
          ) |>
            structure(class = "S7_base_class"),
          list(
            class = "list",
            constructor_name = "list",
            constructor = as.function(list(.data = list(), quote(.data)), envir = baseenv()),
            validator = (function(object) {
              if (base_class(object) != name) {
                sprintf("Underlying data must be <%s> not <%s>", name, base_class(object))
              }
            }) |>
              (`environment<-`)(constructive::.env("0x123456789", parents = "namespace:S7"))
          ) |>
            structure(class = "S7_base_class")
        )
      ) |>
        structure(class = "S7_union")

# S7_union, S7 > 0.2.2

    Code
      construct(S7::class_vector, opts_S7_union("next"), opts_S7_base_class("next"),
      check = FALSE)
    Output
      list(
        classes = list(
          list(
            class = "logical",
            constructor_name = "logical",
            constructor = as.function(list(.data = logical(0), quote(.data)), envir = asNamespace("S7")),
            validator = (function(object) if (base_class(object) != "logical") {
              sprintf("Underlying data must be <%s> not <%s>", "logical", base_class(object))
            }) |>
              (`environment<-`)(asNamespace("S7"))
          ) |>
            structure(class = "S7_base_class"),
          list(
            class = "integer",
            constructor_name = "integer",
            constructor = as.function(list(.data = integer(0), quote(.data)), envir = asNamespace("S7")),
            validator = (function(object) if (base_class(object) != "integer") {
              sprintf("Underlying data must be <%s> not <%s>", "integer", base_class(object))
            }) |>
              (`environment<-`)(asNamespace("S7"))
          ) |>
            structure(class = "S7_base_class"),
          list(
            class = "double",
            constructor_name = "double",
            constructor = as.function(list(.data = numeric(0), quote(.data)), envir = asNamespace("S7")),
            validator = (function(object) if (base_class(object) != "double") {
              sprintf("Underlying data must be <%s> not <%s>", "double", base_class(object))
            }) |>
              (`environment<-`)(asNamespace("S7"))
          ) |>
            structure(class = "S7_base_class"),
          list(
            class = "complex",
            constructor_name = "complex",
            constructor = as.function(list(.data = complex(0), quote(.data)), envir = asNamespace("S7")),
            validator = (function(object) if (base_class(object) != "complex") {
              sprintf("Underlying data must be <%s> not <%s>", "complex", base_class(object))
            }) |>
              (`environment<-`)(asNamespace("S7"))
          ) |>
            structure(class = "S7_base_class"),
          list(
            class = "character",
            constructor_name = "character",
            constructor = as.function(list(.data = character(0), quote(.data)), envir = asNamespace("S7")),
            validator = (function(object) if (base_class(object) != "character") {
              sprintf("Underlying data must be <%s> not <%s>", "character", base_class(object))
            }) |>
              (`environment<-`)(asNamespace("S7"))
          ) |>
            structure(class = "S7_base_class"),
          list(
            class = "raw",
            constructor_name = "raw",
            constructor = as.function(list(.data = raw(0), quote(.data)), envir = asNamespace("S7")),
            validator = (function(object) if (base_class(object) != "raw") {
              sprintf("Underlying data must be <%s> not <%s>", "raw", base_class(object))
            }) |>
              (`environment<-`)(asNamespace("S7"))
          ) |>
            structure(class = "S7_base_class"),
          list(
            class = "expression",
            constructor_name = "expression",
            constructor = as.function(list(.data = expression(), quote(.data)), envir = asNamespace("S7")),
            validator = (function(object) if (base_class(object) != "expression") {
              sprintf("Underlying data must be <%s> not <%s>", "expression", base_class(object))
            }) |>
              (`environment<-`)(asNamespace("S7"))
          ) |>
            structure(class = "S7_base_class"),
          list(
            class = "list",
            constructor_name = "list",
            constructor = as.function(list(.data = list(), quote(.data)), envir = asNamespace("S7")),
            validator = (function(object) if (base_class(object) != "list") {
              sprintf("Underlying data must be <%s> not <%s>", "list", base_class(object))
            }) |>
              (`environment<-`)(asNamespace("S7"))
          ) |>
            structure(class = "S7_base_class")
        )
      ) |>
        structure(class = "S7_union")

