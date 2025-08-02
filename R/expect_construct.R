expect_construct <- function(x, expected, ...) {
  if (!missing(expected) && rlang::is_na(expected)) {
    out <- construct(x, check = FALSE, ...)$code
    getFromNamespace("write_clip", "clipr")(out)
    return(out)
  }

  expected <- if (missing(expected)) substitute(x) else substitute(expected)
  new_code <- construct(x, check = FALSE, ...)$code
  recreated <- parse(text=new_code)[[1]]
  expect_equal(recreated, expected)
}
