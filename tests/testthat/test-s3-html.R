test_that("html", {
  skip_if_not_installed("htmltools")
  expect_snapshot({
    construct(htmltools::HTML("<b>x</b>"))
    construct(htmltools::HTML("<b>x</b>"), opts_html("next"))
    construct(htmltools::HTML(c("a", "b")))
    construct(htmltools::HTML(""))
    construct(htmltools::HTML("x", .noWS = c("before", "after")))
    # attributes to repair
    construct(structure(htmltools::HTML("x"), foo = 1))
    # corrupted
    construct(structure(c("a", "b"), html = TRUE, class = c("html", "character")))
  })
})
