expect_construct <- function(x, expected, ...) {
  withr::local_envvar(c(TESTTHAT = "true"))
  if (!missing(expected) && rlang::is_na(substitute(expected))) {
    out <- construct(x, check = FALSE, ...)$code
    getFromNamespace("write_clip", "clipr")(out)
    return(out)
  }

  expected <- if (missing(expected)) substitute(x) else substitute(expected)
  new_code <- eval(substitute(construct(x, check = FALSE, ...)$code), parent.frame())
  recreated <- parse(text=new_code)[[1]]
  expect_equal(recreated, expected)
}
