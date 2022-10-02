# maybe that'd be clearer to separate the continuous, discrete, binned

#' @export
construct_idiomatic.Scale <- function(x, ...) {
  args <- as.list(x$call)[-1]
  fun_chr <- rlang::expr_deparse(x$call[[1]])
  fun_defaults <- head(as.list(getFromNamespace(fun_chr, "ggplot2")), -1)
  fun_defaults <- Filter(function(x) !identical(x, quote(expr=)), fun_defaults)
  fun_defaults <- lapply(fun_defaults, eval, asNamespace("ggplot2"))
  if ("trans" %in% names(args) && is.character(fun_defaults$trans)) {
    # might be non robust, adress in time
    fun_defaults$trans <- getFromNamespace(paste0(fun_defaults$trans, "_trans"), "scales")()
  }
  values <- as.list(x)
  args_are_defaults <- mapply(identical, fun_defaults, values[names(fun_defaults)], ignore.environment = TRUE)
  args[names(args_are_defaults)[args_are_defaults]] <- NULL

  # fetch values from ggproto object except special values
  values <- values[setdiff(names(args), c("super", "palette"))]

  # deal with rescaler
  scales_ns <- asNamespace("scales")
  if (identical(environment(values$rescaler), scales_ns)) {
    scales_funs <- mget(getNamespaceExports("scales"), scales_ns)
    fun <- sapply(scales_funs, identical, values$rescaler, ignore.environment = TRUE)
    args$rescaler <- paste0("scales::", names(fun[fun][1]))
    values$rescaler <- NULL
  }

  # construct those
  args[names(values)] <- lapply(values, construct_raw, ...)
  # construct special args
  if ("palette" %in% names(args)) {
    # this might call functions from {scales} which are closures so hard to replicate,
    # easier to just eval in ggplot's namespace, brittle if user provides own palette
    # but do they do that ?

    # we might also check parent.env(environment(values$palette)) and if we're in scales, adjust the code

    # if (identical(args$palette, quote(identity))) "identity" else construct_raw(x$palette, ...)

    # this doesnt work either because the call might contain variables define in closure, as in abs_area(max_size)
    # args$palette <- sprintf('eval(%s, asNamespace("ggplot2"))', rlang::expr_deparse(args$palette))

    # Let's try with ugly heuristics and see if we find better later
    if (identical(args$palette, quote(identity))) {
      args$palette <- "identity"
    } else if (identical(args$palette, quote(abs_area(max_size)))) {
      args$palette <- sprintf("scales::abs_area(%s)", environment(as.list(x)$palette)$max)
    } else {
      args$palette <- construct_raw(as.list(x)$palette)
    }
  }
  if ("super" %in% names(args)) {
    # not sure if robust, but if not we'll adress in due time!
    args$super <- paste0("ggplot2:::", rlang::expr_deparse(args$super)) # not robust if several scales :construct_idiomatic.ggproto(x$super(), ...)
  }

  ## build call
  construct_apply(args, fun = fun_chr, language = TRUE, ...)
}


