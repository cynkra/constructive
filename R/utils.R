# Functions that are used in several places, or that have a general scope

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
perfect_match <- function(needle, haystack) {
  ind <- vapply(haystack, identical, needle, FUN.VALUE = logical(1))
  if (any(ind)) names(haystack[ind])[1]
}

flex_match <- function(needle, haystack) {
  # ignore attributes of needle and its environment-ness
  if (is.environment(needle)) needle <- as.list.environment(needle)
  attributes(needle) <- attributes(needle)["names"]
  # like identical but ignoring attributes of haystack elements and their environment-ness
  identical2 <- function(x, needle) {
    # as.list() doesn't work on environments with a S3 class excluding "environment"
    if (is.environment(x)) x <- as.list.environment(x)
    attributes(x) <- attributes(x)["names"]
    identical(x, needle)
  }
  ind <- vapply(haystack, identical2, needle, FUN.VALUE = logical(1))
  if (any(ind)) names(haystack[ind])[1]
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

# like rlang::env_clone but sets the class
env_clone <- function(x) {
  out <-  rlang::env_clone(x)
  #attributes(out) <- attributes(x)
  out
}

scrub_ggplot <- function(x) {
  x$scales <- env_clone(x$scales)
  x$coordinates <- env_clone( x$coordinates)
  x$facet <- env_clone(x$facet)
  x$plot_env <- NULL

  for (i in seq_along(x$layers)) {
    x$layers[[i]] <- env_clone(x$layers[[i]])
    x$layers[[i]]$constructor <- NULL
    x$layers[[i]]$super <- NULL
    x$layers[[1]]$computed_geom_params <- NULL
    x$layers[[1]]$computed_mapping <- NULL
    x$layers[[1]]$computed_stat_params <- NULL
    # attr(x$layers[[i]]$stat_params$formula, '.Environment') <- NULL
    # attr(x$layers[[i]]$mapping$colour, '.Environment') <- NULL
    # attr(x$layers[[i]]$computed_mapping$x, '.Environment') <- NULL
    # attr(x$layers[[i]]$computed_mapping$y, '.Environment') <- NULL
    # attr(x$layers[[1]]$mapping$fill, '.Environment') <- NULL
  }

  environment(x$scales$super) <- emptyenv() #  env_clone(environment(x$scales$super))
  for (i in seq_along(x$scales$scales)) {
    x$scales$scales[[i]] <- env_clone(x$scales$scales[[i]])
    environment(x$scales$scales[[i]]$super) <- emptyenv()
    x$scales$scales[[i]]$call <- NULL
    # the following line corrupts the plot
    #x$scales$scales[[i]]$super <- NULL
  }

  for (var in names(x$mapping)) {
    attr(x$mapping[[var]], '.Environment') <- NULL
  }

  environment(x$facet$super) <- emptyenv() # env_clone(environment(x$facet$super))
  #if (exists("super", x$facet)) environment(x$facet$super) <- emptyenv()
  if (length(x$facet$params$facets)) {
    x$facet$params$facets[] <- lapply(x$facet$params$facets, function(x) {
      attr(x, '.Environment') <- NULL
      x
    })
  }

  # x$facet$super <- NULL
  # x$scales$super <- NULL
  # x$coordinates$super <- NULL
  # x$coordinates$default <- NULL
  x
}

compare_proxy_ggplot <- function(x, path) {
  list(object = scrub_ggplot(x), path = path)
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

snakeize <- function (x) {
  x <- gsub("([A-Za-z])([A-Z])([a-z])", "\\1_\\2\\3", x)
  x <- gsub(".", "_", x, fixed = TRUE)
  x <- gsub("([a-z])([A-Z])", "\\1_\\2", x)
  tolower(x)
}
