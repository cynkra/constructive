colon_colon <- `::`

`::` <- function(x, y) {
  x_sym <- ensym(x)
  y_sym <- ensym(y)
  tryCatch(
    inject(colon_colon(!!x_sym, !!y_sym)),
    packageNotFoundError = function(e) {
      skip_if_not_installed(as_string(x_sym))
    }
  )
}

# we want regular behavior of internal generics in the tests
`[` <- base::`[`
`$` <- base::`$`
length <- base::length

expect_snapshot <- function(code) {
  eval.parent(substitute(
    testthat::expect_snapshot(
      code,
      transform = function(out) {
        out <- gsub("%>%", "|>", out, fixed = TRUE)
        out <- gsub("= [.]([,)])", "= _\\1", out)
        out
      }
    )
  ))
}
# have a copy in the global env for some examples in CI
.GlobalEnv$expect_snapshot <- expect_snapshot
