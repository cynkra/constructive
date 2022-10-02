#' @export
construct_idiomatic.Layer <- function(x, ...) {
  params <- c(x$geom_params, x$stat_params, x$aes_params)
  params <- params[unique(names(params))]

  x <- as.list(x)

  key_glyph <- construct_glyph(x$geom$draw_key)
  ggproto.ignore_draw_key <- !is.null(key_glyph)
  geom <- construct_raw(
    x$geom,
    ggproto.ignore_draw_key = ggproto.ignore_draw_key,
    ggproto.simplify_to_string = TRUE,
    ...
  )
  # FIXME: if key_glyph is the default for a given geom it shouldn't be mentioned
  # we should fetch the default glyphs on load or on add

  # it does make a difference, by modifying proto items for some reason
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
    key_glyph = NULL # placeholder
    # only for internal use, I don't think we'll need that
    # layer_class =
  )


  # remove if same as default
  if (is.null(args$geom)) args$geom <- NULL
  if (is.null(args$stat)) args$stat <- NULL
  if (is.null(args$data) || inherits(args$data, "waiver")) args$data <- NULL
  if (is.null(args$mapping)) args$mapping <- NULL
  if (is.null(args$position)) args$position <- NULL
  if (!length(params)) args$params <- NULL
  if (isTRUE(args$inherit.aes)) args$inherit.aes <- NULL
  if (rlang::is_na(args$show.legend)) args$show.legend <- NULL
  if (is.null(key_glyph)) args$key_glyph <- NULL

  args_chr <- lapply(args, construct_raw, ggproto.simplify_to_string = TRUE, ...)
  args_chr$key_glyph <- key_glyph
  args_chr$geom <- geom

  ## build call
  construct_apply(args_chr, fun = "ggplot2::layer", language = TRUE, ggproto.simplify_to_string = TRUE, ...)
}

construct_glyph <- function(draw_key) {
  if (is.null(draw_key)) return(NULL)

  key_glyph <- draw_key

  # FIXME: precompute a list of draw_key funs on load
  ns <- asNamespace("ggplot2")
  match <- data_match(key_glyph, mget(ls(ns, pattern = "^draw_key_"), ns))
  if (!is.null(match)) {
    key_glyph <- sub("^draw_key_", "", match)
    return(sprintf('"%s"', key_glyph))
  }
  for (pkg in globals$ggpackages[-1]) {
    # FIXME: compute a list of draw_key funs when adding packages
    ns <- asNamespace(pkg)
    match <- data_match(key_glyph, mget(ls(ns, pattern = "^draw_key_"), ns))
    if (!is.null(match)) return(sprintf("%s::%s"), pkg, match)
  }

  e <- environment(draw_key)
  if ("f" %in% ls(e)) {
    key_glyph <- eval(quote(f), environment(draw_key))

    ns <- asNamespace("ggplot2")
    match <- data_match(key_glyph, mget(ls(ns, pattern = "^draw_key_"), ns))
    if (!is.null(match)) {
      key_glyph <- sub("^draw_key_", "", match)
      return(sprintf('"%s"', key_glyph))
    }
    for (pkg in globals$ggpackages[-1]) {
      # FIXME: compute a list of draw_key funs when adding packages
      ns <- asNamespace(pkg)
      match <- data_match(key_glyph, mget(ls(ns, pattern = "^draw_key_"), ns))
      if (!is.null(match)) return(sprintf("%s::%s"), pkg, match)
    }
  }

  construct_raw(key_glyph)
}
