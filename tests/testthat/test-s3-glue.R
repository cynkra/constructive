test_that("glue", {
  skip_if_not_installed("glue")
  expect_snapshot({
    construct(glue::as_glue("hello"))
    construct(glue::as_glue(c(a = "hello", b = NA)))
    construct(glue::as_glue(character()))
    construct(glue::as_glue("multi\nline\nstring"))
    construct(glue::as_glue("multi\nline\nstring"), opts_character(multiline = TRUE))
    construct(glue::as_glue(letters), opts_character(trim = 2))
    construct(structure(glue::as_glue("hello"), foo = 1))
    # as_glue() would convert to UTF-8, so we fall back to next
    construct(structure(`Encoding<-`("caf\xe9", "latin1"), class = c("glue", "character")))
    construct(glue::as_glue(c(a = "hello", b = NA)), opts_glue("next"))
    construct(glue::as_glue(character()), opts_glue("next"))
  })
})
