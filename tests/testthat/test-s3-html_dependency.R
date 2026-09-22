test_that("html_dependency", {
  skip_if_not_installed("htmltools")
  expect_snapshot({
    construct(htmltools::htmlDependency("foo", "1.0", src = "www", script = "a.js"))
    construct(
      htmltools::htmlDependency("foo", "1.0", src = "www", script = "a.js"),
      opts_html_dependency("next")
    )
    construct(htmltools::htmlDependency(
      "foo", "1.0",
      src = c(file = "www", href = "https://example.org"),
      meta = list(viewport = "width=device-width"),
      stylesheet = c("a.css", "b.css"),
      head = "<script></script>",
      attachment = "a.txt",
      package = "constructive",
      all_files = FALSE
    ))
    construct(htmltools::htmlDependency(
      "foo", 1,
      src = list(href = "https://example.org"),
      script = list(src = "a.js", type = "module")
    ))
    # attributes to repair
    construct(structure(htmltools::htmlDependency("foo", "1.0", src = "www"), foo = 1))
  })
})
