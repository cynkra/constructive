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

namespace_as_list <- function(pkg, main) {
  ns <- asNamespace(pkg)
  if (pkg == "base") return(as.list(ns))
  objs <- c(
    mget(getNamespaceExports(ns), ns, inherits = TRUE, ifnotfound = list(NULL)),
    as.list(.getNamespaceInfo(ns, "lazydata"))
  )
  if (!main) {
    names(objs) <- paste0(pkg, "::", names(objs))
  }
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

with_s3_method <- function(
    genname,
    class,
    method,
    envir,
    expr
) {
  ns <- asNamespace(envir)
  method_name <- paste0(genname, ".", class)
  method_exists <- exists(method_name, ns$.__S3MethodsTable__.)
  on.exit({
    if (method_exists) {
      ns$.__S3MethodsTable__.[[method_name]] <- initial_method
    } else {
      ns$.__NAMESPACE__.$S3methods <-
        ns$.__NAMESPACE__.$S3methods[ns$.__NAMESPACE__.$S3methods[,3] != method_name]
      lockBinding(".__NAMESPACE__.", ns)
      rm(list = method_name, envir = ns$.__S3MethodsTable__.)
    }
    lockBinding(".__S3MethodsTable__.", ns)
  })
  if(method_exists) {
    initial_method <- ns$.__S3MethodsTable__.[[method_name]]
  } else {
    # edit S3 table
    unlockBinding(".__NAMESPACE__.", ns)
    ns$.__NAMESPACE__.$S3methods <- rbind(
      ns$.__NAMESPACE__.$S3methods,
      c(genname, class, method, NA_character_)
    )
  }
  unlockBinding(".__S3MethodsTable__.", ns)
  ns$.__S3MethodsTable__.[[method_name]] <- method
  eval(substitute(expr), parent.frame())
}

compare_proxy_ggplot <- function(x, path) {
  x <- rapply(x, function(x) {if (!is.environment(x)) return(x) else rlang::env_clone(x)}, how = "replace")
  for (i in seq_along(x$layers)) {
    x$layers[[i]]$constructor <- NULL
    x$layers[[i]]$super <- NULL
    attr(x$layers[[i]]$stat_params$formula, '.Environment') <- NULL
    attr(x$layers[[i]]$mapping$colour, '.Environment') <- NULL
    attr(x$layers[[i]]$computed_mapping$x, '.Environment') <- NULL
    attr(x$layers[[i]]$computed_mapping$y, '.Environment') <- NULL
    attr(x$layers[[1]]$mapping$fill, '.Environment') <- NULL
  }
  for (i in seq_along(x$scales$scales)) {
    x$scales$scales[[i]]$call <- NULL
    x$scales$scales[[i]]$super <- NULL
  }

  for (var in names(x$mapping)) {
    attr(x$mapping[[var]], '.Environment') <- NULL
  }
  x$plot_env <- NULL
  attr(x$facet$params$facets$class, '.Environment') <- NULL
  attr(x$facet$params$facets$drv, '.Environment') <- NULL
  x$facet$super <- NULL
  x$scales$super <- NULL
  x$coordinates$super <- NULL
  x$coordinates$default <- NULL
  list(object = x, path = path)
}

equivalent_ggplot <- function(x, y) {
  x_tbl <- suppressWarnings(ggplot2::ggplot_gtable(ggplot2::ggplot_build(x)))
  y_tbl <- suppressWarnings(ggplot2::ggplot_gtable(ggplot2::ggplot_build(y)))
  x_unlisted <- gsub( "\\d+", "XXX", unlist(x_tbl))
  y_unlisted <- gsub( "\\d+", "XXX", unlist(y_tbl))
  names(x_unlisted) <- gsub( "\\d+", "XXX", names(x_tbl))
  names(y_unlisted) <- gsub( "\\d+", "XXX", names(y_tbl))
  identical(x_unlisted, y_unlisted)
}

keep_only_non_defaults <- function(x, f) {
  fmls <- Filter(function(x) !identical(x, quote(expr=)), formals(f))
  default_values <- lapply(fmls, eval, environment(f))
  for (nm in names(default_values)) {
    if (identical(x[[nm]], default_values[[nm]])) x[[nm]] <- NULL
  }
  x
}
