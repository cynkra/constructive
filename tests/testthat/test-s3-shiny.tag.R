test_that("shiny.tag", {
  skip_if_not_installed("htmltools")
  expect_snapshot({
    construct(htmltools::tags$div(
      class = "x", class = "y", id = "a", htmltools::tags$p("hi"), "txt", NULL
    ))
    construct(htmltools::tags$div(class = "x", htmltools::tags$p("hi")), opts_shiny.tag("tag"))
    construct(htmltools::tags$div(class = "x", htmltools::tags$p("hi")), opts_shiny.tag("next"))
    construct(htmltools::tags$div())
    # an empty named attribs list can't be built with `tag()`
    construct(htmltools::tags$div(), opts_shiny.tag("tag"))
    construct(htmltools::tag("custom", list(a = NULL)))
    # unknown tag names and unnamed attribs use `tag()`
    construct(htmltools::tag("custom", list(a = 1, "b")))
    construct(htmltools::tag("custom", list()))
    construct(htmltools::tag("div", list("b")))
    # non syntactic tag names
    construct(htmltools::tags$`color-profile`("x"))
    # attributes clashing with the tag function's arguments
    construct(htmltools::tag("div", list(.noWS = "a")))
    # .noWS and .renderHook
    construct(htmltools::tags$div(
      checked = NA, `data-x` = 1L, .noWS = c("before", "after"), .renderHook = function(x) x
    ))
    construct(htmltools::tag("custom", list("b"), .noWS = "outside", .renderHook = list(identity, identity)))
    # attributes to repair
    construct(structure(htmltools::tags$div("a"), foo = 1))
  })
})
