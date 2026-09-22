colon_colon <- `::`

`::` <- function(x, y) {
  x_sym <- substitute(x)
  y_sym <- substitute(y)
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
as.character <- base::as.character
as.double <- base::as.double
as.numeric <- base::as.numeric
as.integer <- base::as.integer
as.logical <- base::as.logical
as.complex <- base::as.complex
as.raw <- base::as.raw
is.na <- base::is.na
is.nan <- base::is.nan
is.finite <- base::is.finite
is.infinite <- base::is.infinite
anyNA <- base::anyNA
is.numeric <- base::is.numeric

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
