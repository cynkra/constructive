#' @export
#' @rdname other-opts
opts_Scale <- function(constructor = c("default", "next", "environment"), ...) {
  .cstr_options("Scale", constructor = constructor[[1]], ...)
}

#' @export
#' @method .cstr_construct Scale
.cstr_construct.Scale <- function(x, ...) {
  opts <- list(...)$opts$Scale %||% opts_Scale()
  if (is_corrupted_Scale(x) || opts$constructor == "next") return(NextMethod())
  UseMethod(".cstr_construct.Scale", structure(NA, class = opts$constructor))
}

is_corrupted_Scale <- function(x) {
  # TODO
  FALSE
}

#' @export
#' @method .cstr_construct.Scale environment
.cstr_construct.Scale.environment <- function(x, ...) {
  .cstr_construct.environment(x, ...)
}

#' @export
#' @method .cstr_construct.Scale default
.cstr_construct.Scale.default <- function(x, ...) {
  if (with_versions(ggplot2 > "3.5.2")) {
    # FIXME: scales are broken, and are very hard to get right, probably
    # need a lot of special casing, we use a naive approach and hope for the best

    # note for later:
    # scale_* functions forward args to more general functions like continuous_scale()
    # but on the way they process args in ways that are not easy to invert, for instance
    # palette = "Spectral".
    # the way we use below uses the NSE "backup" done by ggplot but comes with associated caveats
    # maybe we should fall back to the more general code (but probably often verbose)
    # whenever an arg calls non base functions.
    # it seems the structure of the object has changed too, there's a lot of values under x$palette,
    # and these need sometimes to be renamed or "deprocessed"

    # args_from_palette <- env2list(environment(x$palette))
    # names(args_from_palette)[names(args_from_palette) %in% c("colors", "colours")] <- "palette"
    # for (nm in names(args)) {
    #   args[[nm]] <- args_from_palette[[nm]]
    # }

    call <- base::`[[`(x, "call")
    code <- deparse_call0(call, unicode_representation = list(...)$unicode_representation)
    if (!startsWith(code[[1]], "ggplot2::")) {
      code[[1]] <- paste0("ggplot2::", code[[1]])
    }
    return(code)
  }

  # fetch caller and args from original call
  # here we need the ggplot subsetting method, not the low level [[
  call <- base::`[[`(x, "call")
  caller <- base::`[[`(x, "call")[[1]]
  args <- as.list(call)[-1]
  fun_chr <- rlang::expr_deparse(caller)
  # the caller might be in the form pkg::fun
  fun_val <- eval(caller, asNamespace("ggplot2"))

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
  fun_defaults <- defaults_arg_values(fun_val, "ggplot2")
  if (length(fun_defaults)) {
    if ("trans" %in% names(args) && is.character(fun_defaults$trans)) {
      # might be non robust, address in time
      fun_defaults$trans <- getFromNamespace(paste0(fun_defaults$trans, "_trans"), "scales")()
    }
    args_are_defaults <- mapply(identical, fun_defaults, values[names(fun_defaults)], ignore.environment = TRUE)
    args[names(args_are_defaults)[args_are_defaults]] <- NULL
  }

  # for some reason `values` is not stored in a field of the ggproto object
  # we can fetch it from `palette`
  if (
    !"values" %in% names(values) && # to be safe, might always be TRUE
    "values" %in% names(args)
  ) {
    values$values <- environment(environment(x$palette)$palette)$values
  }

  # fetch values from ggproto object except special values
  values <- values[setdiff(names(args), c("super", "palette", "range"))]

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
  args[names(values)] <- lapply(values, function(x, ...) .cstr_construct(x, ...), ...)

  # special case waiver as it's an empty list unfortunately matched to `.data`
  # FIXME: we should probably not match empty objects, that includes NULL and zero length objects
  if (identical(values$guide, ggplot2::waiver())) {
    args$guide <- "ggplot2::waiver()"
  }

  # construct special args
  if ("range" %in% names(args)) {
    args$range <- .cstr_construct(environment(x$palette)$range, ...)
  }

  if ("palette" %in% names(args)) {
    # FIXME: Simple heuristics for now, can be improved
    if (identical(args$palette, quote(identity))) {
      args$palette <- "identity"
    } else if (identical(args$palette, quote(abs_area(max_size)))) {
      args$palette <- sprintf("scales::abs_area(%s)", environment(as.list(x)$palette)$max)
    } else if (identical(args$palette, quote(rescale_pal(range)))) {
      range_val <- environment(as.list(x)$palette)$range
      if (identical(range_val, defaults_arg_values(scales::rescale_pal, "scales")$range)) {
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
  if (!startsWith(fun_chr, "ggplot2::")) fun_chr <- paste0("ggplot2::", fun_chr)
  .cstr_apply(args, fun = fun_chr, recurse = FALSE, ...)
}

repair_attributes_Scale <- function(x, code, ...) {
  code
}

