# html_dependency

    Code
      construct(htmltools::htmlDependency("foo", "1.0", src = "www", script = "a.js"))
    Output
      htmltools::htmlDependency("foo", "1.0", src = "www", script = "a.js")
    Code
      construct(htmltools::htmlDependency("foo", "1.0", src = "www", script = "a.js"),
      opts_html_dependency("next"))
    Output
      list(
        name = "foo",
        version = "1.0",
        src = list(file = "www"),
        meta = NULL,
        script = "a.js",
        stylesheet = NULL,
        head = NULL,
        attachment = NULL,
        package = NULL,
        all_files = TRUE
      ) |>
        structure(class = "html_dependency")
    Code
      construct(htmltools::htmlDependency("foo", "1.0", src = c(file = "www", href = "https://example.org"),
      meta = list(viewport = "width=device-width"), stylesheet = c("a.css", "b.css"),
      head = "<script></script>", attachment = "a.txt", package = "constructive",
      all_files = FALSE))
    Output
      htmltools::htmlDependency(
        "foo",
        "1.0",
        src = c(file = "www", href = "https://example.org"),
        meta = list(viewport = "width=device-width"),
        stylesheet = c("a.css", "b.css"),
        head = "<script></script>",
        attachment = "a.txt",
        package = "constructive",
        all_files = FALSE
      )
    Code
      construct(htmltools::htmlDependency("foo", 1, src = list(href = "https://example.org"),
      script = list(src = "a.js", type = "module")))
    Output
      htmltools::htmlDependency(
        "foo",
        "1",
        src = c(href = "https://example.org"),
        script = list(src = "a.js", type = "module")
      )
    Code
      construct(structure(htmltools::htmlDependency("foo", "1.0", src = "www"), foo = 1))
    Output
      htmltools::htmlDependency("foo", "1.0", src = "www") |>
        structure(foo = 1)

