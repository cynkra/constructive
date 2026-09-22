# shiny.tag.function

    Code
      construct(htmltools::tagFunction(function() htmltools::tags$b("x")))
    Output
      htmltools::tagFunction(function() htmltools::tags$b("x"))
    Code
      construct(htmltools::tagFunction(function() htmltools::tags$b("x")),
      opts_shiny.tag.function("next"))
    Output
      (function() htmltools::tags$b("x")) |>
        structure(class = "shiny.tag.function")
    Code
      construct(structure(htmltools::tagFunction(function() NULL), foo = 1))
    Output
      htmltools::tagFunction(function() NULL) |>
        structure(foo = 1)

