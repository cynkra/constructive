# shiny.tag.list

    Code
      construct(htmltools::tagList("a", htmltools::tags$b("x")))
    Output
      htmltools::tagList("a", htmltools::tags$b("x"))
    Code
      construct(htmltools::tagList("a", htmltools::tags$b("x")), opts_shiny.tag.list(
        "next"))
    Output
      list("a", htmltools::tags$b("x")) |>
        structure(names = c("", ""), class = c("shiny.tag.list", "list"))
    Code
      construct(htmltools::tagList())
    Output
      htmltools::tagList()
    Code
      construct(htmltools::tagList(a = 1, 2))
    Output
      htmltools::tagList(a = 1, 2)
    Code
      construct(structure(list(.named = 1), class = c("shiny.tag.list", "list")))
    Output
      list(.named = 1) |>
        structure(class = c("shiny.tag.list", "list"))
    Code
      construct(structure(htmltools::tagList("a"), foo = 1))
    Output
      htmltools::tagList("a") |>
        structure(foo = 1)

