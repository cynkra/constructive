#' @export
#' @rdname other-opts
opts_theme <- function(constructor = c("theme", "next", "list"), ...) {
  .cstr_options("theme", constructor = constructor[[1]], ...)
}

#' @export
#' @method .cstr_construct theme
.cstr_construct.theme <- function(x, ...) {
  opts <- list(...)$opts$theme %||% opts_theme()
  if (is_corrupted_theme(x) || opts$constructor == "next") return(NextMethod())
  UseMethod(".cstr_construct.theme", structure(NA, class = opts$constructor))
}

is_corrupted_theme <- function(x) {
  !is.list(x) || !is.null(attr(x, "S7_class"))
}

#' @export
#' @method .cstr_construct.theme list
.cstr_construct.theme.list <- function(x, ...) {
  .cstr_construct.list(x, ...)
}

#' @export
#' @method .cstr_construct.theme theme
.cstr_construct.theme.theme <- function(x, ...) {
  args <- unclass(x)
  args$complete <- if (attr(x, "complete")) TRUE
  args$validate <- if (!attr(x, "validate")) FALSE
  if (attr(x, "complete")) {
    code <- guess_complete_theme(x, ...)
    if (!is.null(code)) {
      code <- repair_attributes_theme(x, code, ...)
      return(code)
    }
  }
  code <- .cstr_apply(args, "ggplot2::theme", ...)
  repair_attributes_theme(x, code, ...)
}

strip_theme <- function(x) {
  # complete themes in ggplot2 all have the same args
  # (in extensions like ggthemes there might be more or less)
  # we scrub their effect so we can compare themes

  # these will mostly be set through the base_size arg
  x$margins <- NULL
  x$spacing <- NULL

  x$legend.margin <- NULL
  x$legend.spacing <- NULL
  x$legend.box.spacing <- NULL
  x$panel.spacing <- NULL
  x$plot.margin <- NULL


  x$strip.switch.pad.grid <- NULL
  x$strip.switch.pad.wrap <- NULL
  x$legend.key.spacing <- NULL

  if (with_versions(ggplot2 <= "3.5.2")) {
    x$text$size <- NULL
    x$text$family <- NULL
    x$rect$linewidth <- NULL
    x$line$linewidth <- NULL


    x$axis.title.x$margin <- NULL
    x$axis.title.x.top$margin <- NULL
    x$axis.title.y$margin <- NULL
    x$axis.title.y.right$margin <- NULL
    x$axis.text.x$margin <- NULL
    x$axis.text.x.top$margin <- NULL
    x$axis.text.y$margin <- NULL
    x$axis.text.y.right$margin <- NULL
    x$axis.text.r$margin <- NULL
    x$axis.ticks.length <- NULL
    x$plot.title$margin <- NULL
    x$plot.subtitle$margin <- NULL
    x$plot.caption$margin <- NULL
    x$strip.text$margin <- NULL
  } else {
    x$text@size <- NULL
    x$text@family <- NULL
    x$rect@linewidth <- NULL
    if (!identical(x$line, ggplot2::element_blank())) {
      x$line@linewidth <- NULL
    }

    if (!is.null(x$axis.title.x)) {
      x$axis.title.x@margin <- NULL
    }

    if (!is.null(x$axis.title.x.top)) {
      x$axis.title.x.top@margin <- NULL
    }

    if (!is.null(x$axis.title.y)) {
      x$axis.title.y@margin <- NULL
    }

    if (!is.null(x$axis.title.y.right)) {
      x$axis.title.y.right@margin <- NULL
    }

    if (!is.null(x$axis.text.x)) {
      x$axis.text.x@margin <- NULL
    }

    if (!is.null(x$axis.text.x.top)) {
      x$axis.text.x.top@margin <- NULL
    }

    if (!is.null(x$axis.text.y)) {
      x$axis.text.y@margin <- NULL
    }

    if (!is.null(x$axis.text.y.right)) {
      x$axis.text.y.right@margin  <- NULL
    }

    if (!is.null(x$axis.text.y.right)) {
      x$axis.text.y.right@margin  <- NULL
    }

    if (!is.null(x$axis.text.r)) {
      x$axis.text.r@margin  <- NULL
    }

    x$axis.ticks.length <- NULL
    x$plot.title@margin <- NULL
    x$plot.subtitle@margin <- NULL
    x$plot.caption@margin <- NULL

    if (!is.null(x$strip.text)) {
      x$strip.text@margin <- NULL
    }

    if (!identical(x$point, ggplot2::element_blank())) {
      x$point@size <- NULL
      x$point@stroke <- NULL
    }

    if (!identical(x$polygon, ggplot2::element_blank())) {
      x$polygon@linewidth  <- NULL
    }
    x$geom@fontsize <- NULL
    x$geom@pointsize <- NULL

    x$geom@linewidth <- NULL
    x$geom@borderwidth <- NULL
  }

  x
}

guess_complete_theme <- function(x, ...) {
  ns <- asNamespace("ggplot2")
  complete_themes <- c(
    "theme_bw", "theme_classic", "theme_dark", "theme_get", "theme_gray",
    "theme_grey", "theme_light", "theme_linedraw", "theme_minimal", "theme_void"
  )
  x_stripped <- strip_theme(x)
  # FIXME: we don't check if x$axis.title.x$margin etc have been set to other than
  # normal defaults for complete themes so some contrived corner cases are not covered
  for (th in complete_themes) {
    th_val <- ns[[th]]()
    th_val_stripped <- strip_theme(th_val)
    if (identical(th_val_stripped, x_stripped)) {
      args <- list()

      if (with_versions(ggplot2 <= "3.5.2")) {
        if (th_val$text$size != x$text$size) {
          args$base_size <- x$text$size
        }
        if (th_val$text$family != x$text$family) {
          args$base_family <- x$text$family
        }
        if (x$line$linewidth != x$text$size / 22) {
          args$base_line_size <- x$line$linewidth
        }
        if (x$rect$linewidth != x$text$size / 22) {
          args$base_rect_size <- x$rect$linewidth
        }
      } else {
        if (th_val$text@size != x$text@size) {
          args$base_size <- x$text@size
        }
        if (th_val$text@family != x$text@family) {
          args$base_family <- x$text@family
        }
        if (x$line@linewidth != x$text@size / 22) {
          args$base_line_size <- x$line@linewidth
        }
        if (x$rect@linewidth != x$text@size / 22) {
          args$base_rect_size <- x$rect@linewidth
        }
      }

      code <- .cstr_apply(args, paste0("ggplot2::", th), ...)
      return(code)
    }
  }
  NULL
}

repair_attributes_theme <- function(x, code, ...) {
  ignore <- c("complete", "validate")
  if (identical(names(x), character())) ignore <- c(ignore, "names")
  .cstr_repair_attributes(x, code, idiomatic_class = c("theme", "gg"), ignore = ignore, ...)
}
