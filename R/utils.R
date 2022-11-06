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
  objs <- c(
    mget(getNamespaceExports(ns), ns, inherits = TRUE, ifnotfound = list(NULL)),
    as.list(.getNamespaceInfo(ns, "lazydata"))
  )
  names(objs) <- paste0(pkg, "::", names(objs))
  objs
}

# much faster than match()
match2 <- function(needle, haystack) {
  # ignore attributes of needle and its environment-ness
  if (is.environment(needle)) needle <- as.list(needle)
  attributes(needle) <- attributes(needle)["names"]
  # like identical but ignoring attributes of haystack elements and their environment-ness
  identical2 <- function(x, needle) {
    # as.list() doesn't work on environments with a S3 class excluding "environment"
    if (is.environment(x)) x <- as.list.environment(x)
    attributes(x) <- attributes(x)["names"]
    identical(x, needle)
  }
  ind <- which(vapply(haystack, identical2, needle, FUN.VALUE = logical(1)))
  if (length(ind)) ind <- ind[[1]]
  ind
}


# adapted from glue::glue_collapse
collapse <- function (x, sep = ",", width = 80, last = " and ", quote = "") {
  if (length(x) == 0) {
    return(character())
  }
  if (any(is.na(x))) {
    return(NA_character_)
  }
  x <- paste0(quote, x, quote)
  if (nzchar(last) && length(x) > 1) {
    res <- collapse(x[seq(1, length(x) - 1)], sep = sep, width = Inf, last = "")
    return(collapse(paste0(res, last, x[length(x)]), width = width))
  }
  x <- paste0(x, collapse = sep)
  if (width < Inf) {
    x_width <- nchar(x, "width")
    too_wide <- x_width > width
    if (too_wide) {
      x <- paste0(substr(x, 1, width - 3), "...")
    }
  }
  x
}
