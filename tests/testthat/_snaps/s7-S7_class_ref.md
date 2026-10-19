# S7_class_ref

    Code
      Dog <- evalq(S7::new_class("Dog", properties = list(name = S7::class_character)),
      .GlobalEnv)
      construct(Dog(name = "x"), check = FALSE)
    Output
      S7::S7_object() |>
        structure(
          class = c("Dog", "S7_object"),
          `_S7_class` = environment(
            S7::new_class(
              "Dog",
              parent = S7::S7_object,
              properties = list(name = S7::class_character),
              constructor = as.function(
                list(
                  name = character(0),
                  quote({
                    name
                    S7::new_object(S7::S7_object(), name = name)
                  })
                ),
                envir = .GlobalEnv
              ) |>
                structure(class = "S7_constructor")
            )
          )$.S7_class_ref,
          name = "x"
        )
    Code
      construct(attr(Dog(name = "x"), "_S7_class"), check = FALSE)
    Output
      environment(
        S7::new_class(
          "Dog",
          parent = S7::S7_object,
          properties = list(name = S7::class_character),
          constructor = as.function(
            list(
              name = character(0),
              quote({
                name
                S7::new_object(S7::S7_object(), name = name)
              })
            ),
            envir = .GlobalEnv
          ) |>
            structure(class = "S7_constructor")
        )
      )$.S7_class_ref
    Code
      construct(attr(Dog(name = "x"), "_S7_class"), opts_S7_class_ref("next"), check = FALSE)
    Output
      constructive::.env("0x123456789", parents = "empty", class = "S7_class_ref")

