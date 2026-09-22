# glue

    Code
      construct(glue::as_glue("hello"))
    Output
      glue::as_glue("hello")
    Code
      construct(glue::as_glue(c(a = "hello", b = NA)))
    Output
      glue::as_glue(c(a = "hello", b = NA))
    Code
      construct(glue::as_glue(character()))
    Output
      glue::as_glue(character(0))
    Code
      construct(glue::as_glue("multi\nline\nstring"))
    Output
      glue::as_glue("multi\nline\nstring")
    Code
      construct(glue::as_glue("multi\nline\nstring"), opts_character(multiline = TRUE))
    Output
      glue::as_glue("multi
      line
      string")
    Code
      construct(glue::as_glue(letters), opts_character(trim = 2))
    Message
      {constructive} couldn't create code that reproduces perfectly the input
      i Call `construct_issues()` to inspect the last issues
    Output
      glue::as_glue(c("a", "b", character(24)))
    Code
      construct(structure(glue::as_glue("hello"), foo = 1))
    Output
      glue::as_glue("hello") |>
        structure(foo = 1)
    Code
      construct(latin1_glue)
    Output
      "caf\xe9" |> (`Encoding<-`)("latin1") |>
        structure(class = c("glue", "character"))
    Code
      construct(glue::as_glue(c(a = "hello", b = NA)), opts_glue("next"))
    Output
      c(a = "hello", b = NA) |>
        structure(class = c("glue", "character"))
    Code
      construct(glue::as_glue(character()), opts_glue("next"))
    Output
      character(0) |>
        structure(class = c("glue", "character"))

