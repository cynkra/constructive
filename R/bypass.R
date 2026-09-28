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

vapply <- function(X, FUN, FUN.VALUE, ..., USE.NAMES = TRUE) {
  base::vapply(unclass(X), FUN, FUN.VALUE, ..., USE.NAMES = USE.NAMES)
}

# coercion and predicates =====================================================
# These are internal generics: they dispatch on the class of objects. We call the
# base version on the unclassed object, which gives the default behaviour.
# Note that `as.numeric()` is `as.double()`, methods are defined for the latter.

as.character <- function(x, ...) base::as.character(unclass(x), ...)

as.double <- function(x, ...) base::as.double(unclass(x), ...)

as.numeric <- as.double

as.integer <- function(x, ...) base::as.integer(unclass(x), ...)

as.logical <- function(x, ...) base::as.logical(unclass(x), ...)

as.complex <- function(x, ...) base::as.complex(unclass(x), ...)

as.raw <- function(x) base::as.raw(unclass(x))

is.na <- function(x) base::is.na(unclass(x))

is.nan <- function(x) base::is.nan(unclass(x))

is.finite <- function(x) base::is.finite(unclass(x))

is.infinite <- function(x) base::is.infinite(unclass(x))

anyNA <- function(x, recursive = FALSE) base::anyNA(unclass(x), recursive = recursive)

is.numeric <- function(x) base::is.numeric(unclass(x))

# dimensions ===================================================================

length <- function(x) {
  if (is.environment(x)) return(base::length(ls(x, all.names = TRUE)))
  base::length(unclass(x))
}

lengths <- function(x, use.names = TRUE) {
  out <- vapply(x, length, integer(1), USE.NAMES = FALSE)
  if (use.names) names(out) <- names(x)
  out
}

dim <- function(x) {
  attr(x, "dim")
}

`dim<-` <- function(x, value) {
  attr(x, "dim") <- value
  x
}

dimnames <- function(x) {
  attr(x, "dimnames")
}

`dimnames<-` <- function(x, value) {
  attr(x, "dimnames") <- value
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

`[[` <- function(x, ...) {
  .subset2(x, ...)
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

