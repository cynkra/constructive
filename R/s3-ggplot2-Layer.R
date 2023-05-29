#' Constructive options for class 'Layer' (ggplot2)
#'
#' These options will be used on objects of class 'Layer'.
#'
#' Depending on `constructor`, we construct the object as follows:
#' * `"default"` : We attempt to use the function originally used to create the
#'   plot.
#' * `"layer"` : We use the `ggplot2::layer()` function
#' * `"environment"` : Reconstruct the object using the general environment method
#'  (which can be itself tweaked using `opts_environment()`)
#'
#'  The latter constructor is the only one that reproduces the object exactly
#'  since Layers are environments and environments can't be exactly copied (see `?opts_environment`)
#'
#' @param constructor String. Name of the function used to construct the environment, see Details section.
#' @inheritParams opts_atomic
#' @return An object of class <constructive_options/constructive_options_Layer>
#' @export
opts_Layer <- function(constructor = c("default", "layer", "environment"), ...) {
  .cstr_combine_errors(
    constructor <- rlang::arg_match(constructor),
    ellipsis::check_dots_empty()
  )
  .cstr_options("Layer", constructor = constructor)
}

#' @export
.cstr_construct.Layer <- function(x, ..., env) {
  opts <- .cstr_fetch_opts("Layer", ...)

  ## "environment" constructor
  if (opts$constructor == "environment") {
    return(.cstr_construct.environment(x, ...))
  }

  ## "default" constructor
  if (opts$constructor == "default" && !is.null(x$constructor)) {
    return(construct_layer_default(x$constructor, env, ...))
  }

  ## "layer" constructor (and fallback from "default")
  construct_layer_layer(x, ...)
}

construct_layer_default <- function(constructor, env, ...) {
  caller_lng <- constructor[[1]]
  caller_val <- eval(caller_lng, env)
  ns <-  topenv(environment(caller_val)) # the most likely namespace
  if (isNamespace(ns) && rlang::is_call(constructor, ns = getNamespaceName(ns))) {
    caller_chr <- deparse_call(caller_lng)
  } else if (is.symbol(caller_lng) && isNamespace(ns) && as.character(caller_lng) %in% getNamespaceExports(ns)) {
    caller_chr <- paste0(getNamespaceName(ns), "::", as.character(caller_lng))
  } else {
    caller_chr <- .cstr_construct(caller_val, env = env, ...)
  }
  args <- lapply(as.list(constructor)[-1], eval, env)
  args <- keep_only_non_defaults(args, caller_val)
  .cstr_apply(args, caller_chr, env = env, ...)
}

construct_layer_layer <- function(x, ...) {
  # reconstruct the parameters from layer()
  # layer(
  #   geom = NULL, stat = NULL, data = NULL, mapping = NULL, position = NULL,
  #   params = list(), inherit.aes = TRUE, check.aes = TRUE, check.param = TRUE,
  #   show.legend = NA, key_glyph = NULL, layer_class = Layer
  # )

  # params ---------------------------------------------------------------------
  # params is split in 3 parts in the Layer object, we bind them back together
  params <- c(x$geom_params, x$stat_params, x$aes_params)
  params <- params[unique(names(params))]

  # key_glyph ------------------------------------------------------------------
  # in layer() the raw_key string when given is used to replace geom by a function
  # right before the ggproto call
  x <- as.list(x)
  key_glyph <- construct_glyph(x$geom$draw_key)
  ggproto.ignore_draw_key <- !is.null(key_glyph)

  # geom -----------------------------------------------------------------------
  geom <- .cstr_construct(
    x$geom,
    ggproto.ignore_draw_key = ggproto.ignore_draw_key,
    ...
  )

  # remove key_glyph if same as geom -------------------------------------------
  if (identical(key_glyph, geom)) key_glyph <- NULL

  # other args -----------------------------------------------------------------
  args <- list(
    geom = NULL, # placeholder
    stat = x$stat,
    data = x$data,
    mapping = x$mapping,
    position = x$position,
    params = params,
    inherit.aes = x$inherit.aes,
    # don't make it to the ggproto call, leave as default
    # check.aes =,
    # check.param =,
    show.legend = x$show.legend,
    key_glyph = NULL # placeholders
  )

  # remove if same as default --------------------------------------------------
  if (is.null(args$geom)) args$geom <- NULL
  if (is.null(args$stat))  args$stat <- NULL
  if (is.null(args$data) || inherits(args$data, "waiver")) args$data <- NULL
  if (is.null(args$mapping)) args$mapping <- NULL
  if (is.null(args$position)) args$position <- NULL
  if (!length(params)) args$params <- NULL
  if (isTRUE(args$inherit.aes)) args$inherit.aes <- NULL
  if (rlang::is_na(args$show.legend)) args$show.legend <- NULL
  if (is.null(key_glyph)) args$key_glyph <- NULL
  args_chr <- lapply(args, .cstr_construct, ggproto.ignore_draw_key = ggproto.ignore_draw_key, ...)
  args_chr$key_glyph <- key_glyph
  args_chr$geom <- geom

  ## build call ----------------------------------------------------------------
  .cstr_apply(args_chr, fun = "ggplot2::layer", recurse = FALSE, ggproto.ignore_draw_key = ggproto.ignore_draw_key,  ...)
}


construct_glyph <- function(draw_key) {
  if (is.null(draw_key)) return(NULL)

  key_glyph <- draw_key

  # FIXME: precompute a list of draw_key funs on load
  ns <- asNamespace("ggplot2")
  match <- perfect_match(environment(key_glyph)$f, mget(ls(ns, pattern = "^draw_key_"), ns))
  if (!is.null(match)) {
    key_glyph <- sub("^draw_key_", "", match)
    return(sprintf('"%s"', key_glyph))
  }
  for (pkg in globals$ggpackages[-1]) {
    # FIXME: compute a list of draw_key funs when adding packages
    ns <- asNamespace(pkg)
    match <- flex_match(key_glyph, mget(ls(ns, pattern = "^draw_key_"), ns))
    if (!is.null(match)) return(sprintf("%s::%s"), pkg, match)
  }

  e <- environment(draw_key)
  if ("f" %in% ls(e)) {
    key_glyph <- eval(quote(f), environment(draw_key))

    ns <- asNamespace("ggplot2")
    match <- flex_match(key_glyph, mget(ls(ns, pattern = "^draw_key_"), ns))
    if (!is.null(match)) {
      key_glyph <- sub("^draw_key_", "", match)
      return(sprintf('"%s"', key_glyph))
    }
    for (pkg in globals$ggpackages[-1]) {
      # FIXME: compute a list of draw_key funs when adding packages
      ns <- asNamespace(pkg)
      match <- flex_match(key_glyph, mget(ls(ns, pattern = "^draw_key_"), ns))
      if (!is.null(match)) return(sprintf("%s::%s"), pkg, match)
    }
  }

  .cstr_construct(key_glyph)
}
