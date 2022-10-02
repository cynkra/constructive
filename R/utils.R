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
  if (is_syntactic(name)) return(name)
  paste0("`", name, "`")
}

namespace_as_list <- function(pkg) {
  ns <- asNamespace(pkg)
  if (pkg == "base") return(as.list(ns))

  # this is slow, we should only fetch datasets
  c(
    mget(setdiff(getNamespaceExports(ns), unlist(.getNamespaceInfo(ns, "imports"))), ns),
    # #as.list(.getNamespaceInfo(ns, "imports")),
    as.list(.getNamespaceInfo(ns, "lazydata"))
  )
}

default_args <- function(fun, env = environment(fun)) {
  defaults <- head(as.list(fun), -1)
  defaults <- Filter(function(x) !identical(x, quote(expr=)), defaults)
  # FIXME: might fail if some args depend on previous args
  lapply(defaults, eval, env)
}

keep_only_non_defaults <- function(args, fun, env = environment(fun)) {
  defaults <- default_args(fun, env)
  args_are_defaults <- mapply(identical, defaults, args[names(defaults)], ignore.environment = TRUE)
  args[names(args_are_defaults)[args_are_defaults]] <- NULL
  args
}
