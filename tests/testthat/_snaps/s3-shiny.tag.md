# shiny.tag

    Code
      construct(htmltools::tags$div(class = "x", class = "y", id = "a", htmltools::tags$
        p("hi"), "txt", NULL))
    Output
      htmltools::tags$div(class = "x", class = "y", id = "a", htmltools::tags$p("hi"), "txt", NULL)
    Code
      construct(htmltools::tags$div(class = "x", htmltools::tags$p("hi")),
      opts_shiny.tag("tag"))
    Output
      htmltools::tag("div", list(class = "x", htmltools::tags$p("hi")))
    Code
      construct(htmltools::tags$div(class = "x", htmltools::tags$p("hi")),
      opts_shiny.tag("next"))
    Output
      list(
        name = "div",
        attribs = list(class = "x"),
        children = list(
          list(
            name = "p",
            attribs = list() |>
              structure(names = character(0)),
            children = list("hi")
          ) |>
            structure(class = "shiny.tag")
        )
      ) |>
        structure(class = "shiny.tag")
    Code
      construct(htmltools::tags$div())
    Output
      htmltools::tags$div()
    Code
      construct(htmltools::tags$div(), opts_shiny.tag("tag"))
    Output
      htmltools::tags$div()
    Code
      construct(htmltools::tag("custom", list(a = NULL)))
    Output
      list(
        name = "custom",
        attribs = list() |>
          structure(names = character(0)),
        children = list()
      ) |>
        structure(class = "shiny.tag")
    Code
      construct(htmltools::tag("custom", list(a = 1, "b")))
    Output
      htmltools::tag("custom", list(a = 1, "b"))
    Code
      construct(htmltools::tag("custom", list()))
    Output
      htmltools::tag("custom", list())
    Code
      construct(htmltools::tag("div", list("b")))
    Output
      htmltools::tag("div", list("b"))
    Code
      construct(htmltools::tags$`color-profile`("x"))
    Output
      htmltools::tags$`color-profile`("x")
    Code
      construct(htmltools::tag("div", list(.noWS = "a")))
    Output
      htmltools::tag("div", list(.noWS = "a"))
    Code
      construct(htmltools::tags$div(checked = NA, `data-x` = 1L, .noWS = c("before",
        "after"), .renderHook = function(x) x))
    Output
      htmltools::tags$div(
        checked = NA,
        `data-x` = 1L,
        .noWS = c("before", "after"),
        .renderHook = function(x) x
      )
    Code
      construct(htmltools::tag("custom", list("b"), .noWS = "outside", .renderHook = list(
        identity, identity)))
    Output
      htmltools::tag(
        "custom",
        list("b"),
        .noWS = "outside",
        .renderHook = list(
          (function(x) x) |>
            (`environment<-`)(.BaseNamespaceEnv),
          (function(x) x) |>
            (`environment<-`)(.BaseNamespaceEnv)
        )
      )
    Code
      construct(structure(htmltools::tags$div("a"), foo = 1))
    Output
      htmltools::tags$div("a") |>
        structure(foo = 1)

