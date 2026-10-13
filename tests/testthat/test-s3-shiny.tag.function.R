test_that("shiny.tag.function", {
  skip_if_not_installed("htmltools")
  expect_snapshot({
    construct(htmltools::tagFunction(function() htmltools::tags$b("x")))
    construct(
      htmltools::tagFunction(function() htmltools::tags$b("x")),
      opts_shiny.tag.function("next")
    )
    # attributes to repair
    construct(structure(htmltools::tagFunction(function() NULL), foo = 1))
  })
})
