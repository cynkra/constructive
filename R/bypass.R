# This script defines shims of base R functions that don't trigger S3 dispatch.
# Indeed in this package S3 dispatch is more likely to be accidental than desired.
# When dispatch is actually desired, we should use the `base::fun` form.
# Ultimately we should use the {bypass} package, more specifically `global_bypass()`

# vectors ======================================================================

c <- function(...) base::c(NULL, ...)

unlist <- function(x, recursive = TRUE, use.names = TRUE) {
  base::unlist(unclass(x))
}

lapply <- function(X, FUN, ...) {
  base::lapply(unclass(X), FUN, ...)
}

sapply <- function(X, FUN, ..., simplify = TRUE, USE.NAMES = TRUE) {
  base::sapply(unclass(X), FUN, ..., simplify = simplify, USE.NAMES = USE.NAMES)
}

# dimensions ===================================================================

length <- function(x) {
  if (is.environment(x)) return(base::length(ls(x, all.names = TRUE)))
  base::length(unclass(x))
}

lengths <- function(x, use.names = TRUE) {
  sapply(x, length, USE.NAMES = use.names)
}

dim <- function(x) {
  attr(x, "dim")
}

`dim<-` <- function(x, value) {
  attr(x, "dim") <- value
  x
}

names <- function(x) {
  if (is.environment(x)) return(ls(x, all.names = TRUE, sorted = FALSE))
  base::names(unclass(x))
}

`names<-` <- function(x, value) {
  attr(x, "names") <- value
  x
}

# subset =======================================================================

`$` <- function(e1, e2) {
  .subset2(e1, as.character(substitute(e2)))
}

`[` <- function(x, ...) {
  cl <- oldClass(x)
  x <- unclass(x)
  out <- base::`[`(x, ...)
  oldClass(out) <- cl
  out
}

`[<-` <- function(x, ..., value) {
  cl <- oldClass(x)
  x <- unclass(x)
  x <- base::`[<-`(x, ..., value = value)
  oldClass(x) <- cl
  x
}

`[[<-` <- function(x, ..., value) {
  cl <- oldClass(x)
  x <- unclass(x)
  x <- base::`[[<-`(x, ..., value = value)
  oldClass(x) <- cl
  x
}

`$<-` <- function(e1, e2, value) {
  e1[[as.character(substitute(e2))]] <- value
  e1
}

# comparison ops ===============================================================

`==` <- function(e1, e2) {
  base::`==`(unclass(e1), unclass(e2))
}

`!=` <- function(e1, e2) {
  base::`!=`(unclass(e1), unclass(e2))
}

`>` <- function(e1, e2) {
  base::`>`(unclass(e1), unclass(e2))
}

`<` <- function(e1, e2) {
  base::`<`(unclass(e1), unclass(e2))
}

`>=` <- function(e1, e2) {
  base::`>=`(unclass(e1), unclass(e2))
}

`<=` <- function(e1, e2) {
  base::`<=`(unclass(e1), unclass(e2))
}

`/` <- function(e1, e2) {
  base::`/`(unclass(e1), unclass(e2))
}

