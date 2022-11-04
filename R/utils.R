wrap <- function(x, fun, new_line = FALSE) {
  if (new_line) return(c(paste0(fun, "("), x, ")"))
  x[1] <- paste0(fun, "(", x[1])
  l <- length(x)
  x[l] <- paste0(x[l], ")")
  x
}

# "c(1,2)" to "foo = c(1,2),"
name_and_append_comma <- function(x, nm, implicit_names = FALSE) {
  if (nm != "" && (!implicit_names || !identical(nm, x))) {
    x[1] <- paste(protect(nm), "=", x[1])
  }
  x[length(x)] <- paste0(x[length(x)], ",")
  x
}

pipe <- function(x, y, pipe, one_liner) {
  pipe_symbol <- c(base = "|>", magrittr = "%>%", plus = "+")[[pipe]]
  if (one_liner) return(paste(x, pipe_symbol, y))
  x[length(x)] <- paste(x[length(x)], pipe_symbol)
  c(x, y)
}


is_syntactic <- function(x) {
  x == make.names(x)
}

protect <- function(name) {
  ifelse(is_syntactic(name), name, paste0("`", name, "`"))
}

namespace_as_list <- function(pkg) {
  ns <- asNamespace(pkg)
  if (pkg == "base") return(as.list(ns))
  c(
    mget(setdiff(getNamespaceExports(ns), unlist(.getNamespaceInfo(ns, "imports"))), ns),
    as.list(.getNamespaceInfo(ns, "lazydata"))
  )
}

# much faster than match()
match2 <- function(needle, haystack) {
  # ignore attributes of needle and its environment-ness
  if (is.environment(needle)) needle <- as.list(needle)
  attributes(needle) <- NULL
  # like identical but ignoring attributes of haystack elements and their environment-ness
  identical2 <- function(x, needle) {
    if (is.environment(x)) x <- as.list(x)
    attributes(x) <- NULL
    identical(x, needle)
  }
  which(vapply(haystack, identical2, needle, FUN.VALUE = logical(1)))
}
