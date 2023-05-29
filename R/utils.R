# Functions that are used in several places, or that have a general scope

#' Wrap argument code in function call
#'
#' Exported for custom constructor design. Generally called through `.cstr_apply()`.
#'
#' @param args A character vector containing the code of arguments.
#' @param fun A string. The name of the function to use in the function call.
#'   Use `fun = ""` to wrap in parentheses.
#' @param new_line Boolean. Whether to insert a new line between `"fun("` and the closing `")"`.
#'
#' @return A character vector.
#' @export
.cstr_wrap <- function(args, fun, new_line = FALSE) {
  if (new_line) return(c(paste0(fun, "("), args, ")"))
  args[1] <- paste0(fun, "(", args[1])
  l <- length(args)
  args[l] <- paste0(args[l], ")")
  args
}

# "c(1,2)" to "foo = c(1,2),"
name_and_append_comma <- function(x, nm, implicit_names = FALSE) {
  if (nm != "" && (!implicit_names || !identical(nm, x))) {
    x[1] <- paste(protect(nm), "=", x[1])
  }
  x[length(x)] <- paste0(x[length(x)], ",")
  x
}

#' Insert a pipe between two calls
#'
#' Exported for custom constructor design.
#'
#' @param x A character vector. The code for the left hand side call.
#' @param y A character vector. The code for the right hand side call.
#' @param pipe A string. The pipe to use, `"plus"` is useful for ggplot code.
#' @param one_liner A boolean. Whether to paste `x`, the pipe and `y` together
#' on a same line (provided that `x` and `y` are strings and one liners themselves)
#'
#' @return A character vector
#' @export
#' @examples
#' .cstr_pipe("iris", "head(2)", pipe = "base", one_liner = FALSE)
#' .cstr_pipe("iris", "head(2)", pipe = "base", one_liner = TRUE)
.cstr_pipe <- function(x, y, pipe, one_liner) {
  pipe_symbol <- c(base = "|>", magrittr = "%>%", plus = "+")[[pipe]]
  if (one_liner) return(paste(x, pipe_symbol, y))
  x[length(x)] <- paste(x[length(x)], pipe_symbol)
  c(x, y)
}


is_syntactic <- function(x) {
  x == make.names(x)
}

protect <- function(name) {
  ifelse(is_syntactic(name) | name == "", name, paste0("`", gsub("`", "\\\\`", name), "`"))
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

scrub_ggplot <- function(x) {
  x <- flatten.scales(x)
  x
}

# Thanks to Zi Lin : https://stackoverflow.com/questions/75960769
flatten.scales <- function(gg) {
  # take stock how many different scales are contained within the top-level
  # scale list, & sort their names alphabetically for consistency
  orig.scales <- gg[["scales"]]
  scale.count <- orig.scales$n()
  scale.aesthetics <- lapply(seq_len(scale.count),
                             function(i) orig.scales$scales[[i]]$aesthetics)
  names(scale.aesthetics) <- lapply(scale.aesthetics,
                                    function(x) x[[1]])
  scale.names.sorted <- sort(names(scale.aesthetics))

  # define a new empty scale list ggproto object
  new.scales <- getFromNamespace("scales_list", asNamespace("ggplot2"))()

  # for each scale, traverse up its inheritance tree until we can't go any
  # higher without losing the function call -- i.e. any super's beyond this
  # point are inheritances defined in ggproto (e.g. ScaleContinuousPosition
  # inherits from ScaleContinuous, which in turn inherits from Scale), not
  # inheritances created during cloning of scales within this ggplot object.
  # add that scale to the new scale list.
  for (i in seq_along(scale.names.sorted)) {
    scale.to.add <- orig.scales$get_scales(scale.names.sorted[[i]])
    while ("super" %in% names(scale.to.add)) {
      scale.to.add1 <- scale.to.add$super()
      if (!is.null(scale.to.add1$call)) {
        scale.to.add <- scale.to.add1
      } else {
        break
      }
    }
    new.scales$add(scale.to.add)
  }

  gg[["scales"]] <- new.scales
  return(gg)
}

# Not used yet, should be used in construction code rather than using flatten.scales
# in waldo_proxy methods
trans_order <- function(x) {
  n_layers <- length(x$layers)
  layers <- seq(n_layers)
  names(layers) <- rep("layers", n_layers)

  n_scales <- x$scales$n()
  if (!n_scales) return(layers)
  n_trans <- n_layers + n_scales

  scale_i_reversed <- function(scale) {
    i <- 0
    while ("super" %in% names(scale)) {
      i <- i + 1
      scale <- scale$super()
      if (is.null(scale$call)) break
    }
    i
  }
  scale_order_reversed <- sapply(x$scales$scales, scale_i_reversed)
  scale_order <- n_trans - scale_order_reversed + 1
  layer_order <- setdiff(seq(n_trans), scale_order)
  scales <- seq(n_scales)
  names(scales) <- rep("scales", n_scales)

  c(layers, scales)[order(c(layer_order, scale_order))]
}

compare_proxy_ggplot <- function(x, path) {
  list(object = scrub_ggplot(x), path = path)
}

equivalent_ggplot <- function(x, y) {
  x_tbl <- suppressWarnings(ggplot2::ggplot_gtable(ggplot2::ggplot_build(x)))
  y_tbl <- suppressWarnings(ggplot2::ggplot_gtable(ggplot2::ggplot_build(y)))
  x_unlisted <- gsub("\\d+", "XXX", unlist(x_tbl))
  y_unlisted <- gsub("\\d+", "XXX", unlist(y_tbl))
  names(x_unlisted) <- gsub("\\d+", "XXX", names(x_tbl))
  names(y_unlisted) <- gsub("\\d+", "XXX", names(y_tbl))
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


compare_proxy_weakref <- function(x, path) {
  wr <- list(key = rlang::wref_key(x), value = rlang::wref_value(x))
  list(object = wr, path = path)
}
