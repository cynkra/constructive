expect_construct <- function(x, expected) {
  # for convenience when calling expect_construct(x,) with the comma we print
  # the expr
  if (missing(expected) && length(sys.call()) > 2) {
    construct_call <- substitute(construct(x))
    out <- construct(x, check = FALSE)$code
    getFromNamespace("write_clip", "clipr")(out)
    return(out)
  }

  expected <- if (missing(expected)) substitute(x) else substitute(expected)
  construct_call <- substitute(construct(x))
  new_code <- construct(x, check = FALSE)$code
  recreated <- parse(text=new_code)[[1]]
  expect_equal(recreated, expected)
}
