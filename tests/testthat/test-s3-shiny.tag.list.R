test_that("shiny.tag.list", {
  skip_if_not_installed("htmltools")
  expect_snapshot({
    construct(htmltools::tagList("a", htmltools::tags$b("x")))
    construct(htmltools::tagList("a", htmltools::tags$b("x")), opts_shiny.tag.list("next"))
    construct(htmltools::tagList())
    construct(htmltools::tagList(a = 1, 2))
    # names clashing with `rlang::dots_list()`'s arguments
    construct(structure(list(.named = 1), class = c("shiny.tag.list", "list")))
    # attributes to repair
    construct(structure(htmltools::tagList("a"), foo = 1))
  })
})
