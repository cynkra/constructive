#' @export
.cstr_construct.Scale <- function(x, ...) {
  # fetch caller and args from original call
  caller <- x$call[[1]]
  args <- as.list(x$call)[-1]
  fun_chr <- rlang::expr_deparse(caller)

  # fetch the actual values from the object
  values <- as.list(x)

  # simplify scale when possible
  if (!is.null(values$limits)) {
    candidate <- do.call(ggplot2::xlim, as.list(values$limits))
    xlim_call_lgl <- isTRUE(all.equal(values, as.list(candidate), ignore.environment = TRUE))
    if (xlim_call_lgl) {
      return(.cstr_apply(as.list(values$limits), "ggplot2::xlim", ...))
    }
    candidate <- do.call(ggplot2::ylim, as.list(values$limits))
    ylim_call_lgl <- isTRUE(all.equal(values, as.list(candidate), ignore.environment = TRUE))
    if (ylim_call_lgl) {
      return(.cstr_apply(as.list(values$limits), "ggplot2::ylim", ...))
    }
  }
  # retrieve the defaults of the function, so we can simplify the call
  # and remove arguments that are repeating the defaults
  fun_defaults <- defaults_arg_values(fun_chr, "ggplot2")
  if ("trans" %in% names(args) && is.character(fun_defaults$trans)) {
    # might be non robust, address in time
    fun_defaults$trans <- getFromNamespace(paste0(fun_defaults$trans, "_trans"), "scales")()
  }
  args_are_defaults <- mapply(identical, fun_defaults, values[names(fun_defaults)], ignore.environment = TRUE)
  args[names(args_are_defaults)[args_are_defaults]] <- NULL

  # fetch values from ggproto object except special values
  values <- values[setdiff(names(args), c("super", "palette"))]

  # deal with `rescaler` arg, it's typically the name of a function from "scales"
  # so we fetch it there
  scales_ns <- asNamespace("scales")
  if (identical(environment(values$rescaler), scales_ns)) {
    scales_funs <- mget(getNamespaceExports("scales"), scales_ns)
    fun <- sapply(scales_funs, identical, values$rescaler, ignore.environment = TRUE)
    args$rescaler <- paste0("scales::", names(fun[fun][1]))
    values$rescaler <- NULL
  }

  # construct values, except for `super` and `palette` that we handle specifically after
  args[names(values)] <- lapply(values, .cstr_construct, ...)

  # special case waiver as it's an empty list unfortunately matched to `.data`
  # FIXME: we should probably not match empty objects, that inclused NULL and zero length objects
  if (identical(values$guide, ggplot2::waiver())) {
    args$guide <- "ggplot2::waiver()"
  }

  # construct special args
  if ("palette" %in% names(args)) {
    # FIXME: Simple heuristics for now, can be improved
    if (identical(args$palette, quote(identity))) {
      args$palette <- "identity"
    } else if (identical(args$palette, quote(abs_area(max_size)))) {
      args$palette <- sprintf("scales::abs_area(%s)", environment(as.list(x)$palette)$max)
    } else if (identical(args$palette, quote(rescale_pal(range)))) {
      range_val <- environment(as.list(x)$palette)$range
      if (identical(range_val, defaults_arg_values("rescale_pal", "scales")$range)) {
        args$palette <- "scales::rescale_pal()"
      } else {
        args$palette <- .cstr_apply(list(range_val), "scales::rescale_pal")
      }
    } else {
      args$palette <- .cstr_construct(as.list(x)$palette, ...)
    }
  }
  if ("super" %in% names(args)) {
    # not sure if robust, but if not we'll address in due time!
    args$super <- paste0("ggplot2:::", rlang::expr_deparse(args$super)) # not robust if several scales :construct_raw.ggproto(x$super(), ...)
  }

  ## build call
  fun_chr <- paste0("ggplot2::", fun_chr)
  .cstr_apply(args, fun = fun_chr, recurse = FALSE, ...)
}
