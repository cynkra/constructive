# html

    Code
      construct(htmltools::HTML("<b>x</b>"))
    Output
      htmltools::HTML("<b>x</b>")
    Code
      construct(htmltools::HTML("<b>x</b>"), opts_html("next"))
    Output
      "<b>x</b>" |>
        structure(html = TRUE, class = c("html", "character"))
    Code
      construct(htmltools::HTML(c("a", "b")))
    Output
      htmltools::HTML("a b")
    Code
      construct(htmltools::HTML(""))
    Output
      htmltools::HTML("")
    Code
      construct(htmltools::HTML("x", .noWS = c("before", "after")))
    Output
      htmltools::HTML("x", .noWS = c("before", "after"))
    Code
      construct(structure(htmltools::HTML("x"), foo = 1))
    Output
      htmltools::HTML("x") |>
        structure(foo = 1)
    Code
      construct(structure(c("a", "b"), html = TRUE, class = c("html", "character")))
    Output
      c("a", "b") |>
        structure(html = TRUE, class = c("html", "character"))

