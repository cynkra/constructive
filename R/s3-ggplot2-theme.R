#' @export
.cstr_construct.theme <- function(x, ...) {
  args <- unclass(x)
  args$complete <- if (attr(x, "complete")) TRUE
  args$validate <- if (!attr(x, "validate")) FALSE
  if (attr(x, "complete")) {
    code <- guess_complete_theme(x, ...)
    if (!is.null(code)) return(code)
  }
  .cstr_apply(args, "ggplot2::theme", ...)
}

strip_theme <- function(x) {
  # complete themes in ggplot2 all have the same args
  # (in extensions like ggthemes there might be more or less)
  # we scrub their effect so we can compare thenes
  x$text$size <- NULL
  x$text$family <- NULL
  x$rect$linewidth <- NULL
  x$line$linewidth <- NULL

  # these will mostly be set through the base_size arg
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
  x$legend.margin <- NULL
  x$legend.spacing <- NULL
  x$legend.box.spacing <- NULL
  x$panel.spacing <- NULL
  x$plot.title$margin <- NULL
  x$plot.subtitle$margin <- NULL
  x$plot.caption$margin <- NULL
  x$plot.margin <- NULL
  x$strip.text$margin <- NULL
  x$strip.switch.pad.grid <- NULL
  x$strip.switch.pad.wrap <- NULL
  x$legend.key.spacing <- NULL
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
      code <- .cstr_apply(args, paste0("ggplot2::", th), ...)
      return(repair_attributes_theme(x, code, ...))
    }
  }
  NULL
}
repair_attributes_theme <- function(x, ...) {
  ignore <- c("complete", "validate")
  if (identical(names(x), character())) ignore <- c(ignore, "names")
  .cstr_repair_attributes(x, idiomatic_class = c("theme", "gg"), ignore = ignore, ...)
}

#' @export
.cstr_construct.element_blank <- function(x, ...) {
  code <- "ggplot2::element_blank()"
  repair_attributes_element_blank(x, code, ...)
}

repair_attributes_element_blank <- function(x, ...) {
  .cstr_repair_attributes(x, idiomatic_class = c("element_blank", "element"), ...)
}

#' @export
.cstr_construct.element_grob <- function(x, ...) {
  args <- keep_only_non_defaults(unclass(x), ggplot2::element_grob)
  code <- .cstr_apply(args, "ggplot2::element_grob", ...)
  repair_attributes_element_grob(x, code, ...)
}

repair_attributes_element_grob <- function(x, ...) {
  .cstr_repair_attributes(x, idiomatic_class = c("element_grob", "element"), ...)
}

#' @export
.cstr_construct.element_rect <- function(x, ...) {
  args <- keep_only_non_defaults(unclass(x), ggplot2::element_rect)
  code <- .cstr_apply(args, "ggplot2::element_rect", ...)
  repair_attributes_element_rect(x, code, ...)
}

repair_attributes_element_rect <- function(x, ...) {
  .cstr_repair_attributes(x, idiomatic_class = c("element_rect", "element"), ...)
}

#' @export
.cstr_construct.element_render <- function(x, ...) {
  args <- keep_only_non_defaults(unclass(x), ggplot2::element_render)
  code <- .cstr_apply(args, "ggplot2::element_render", ...)
  repair_attributes_element_render(x, code, ...)
}

repair_attributes_element_render <- function(x, ...) {
  .cstr_repair_attributes(x, idiomatic_class = c("element_render", "element"), ...)
}

#' @export
.cstr_construct.element_text <- function(x, ...) {
  args <- keep_only_non_defaults(unclass(x), ggplot2::element_text)
  code <- .cstr_apply(args, "ggplot2::element_text", ...)
  repair_attributes_element_text(x, code, ...)
}

repair_attributes_element_text <- function(x, ...) {
  .cstr_repair_attributes(x, idiomatic_class = c("element_text", "element"), ...)
}

#' @export
.cstr_construct.element_line <- function(x, ...) {
  args <- keep_only_non_defaults(unclass(x), ggplot2::element_line)
  .cstr_apply(args, "ggplot2::element_line", ...)
}

repair_attributes_element_line <- function(x, ...) {
  .cstr_repair_attributes(x, idiomatic_class = c("element_line", "element"), ...)
}

#' @export
.cstr_construct.simpleUnit <- function(x, ...) {
  lkp <- c(
    npc = 0L, cm = 1L, inches = 2L, mm = 7L, points = 8L, picas = 9L,
    bigpts = 10L, dida = 11L, cicero = 12L, scaledpts = 13L, lines = 3L,
    char = 18L, native = 4L, snpc = 6L, mylines = 103L, mychar = 104L,
    mystrwidth = 105L, mystrheight = 106L
  )
  units <- names(lkp)[match(attr(x, "unit"), lkp)]
  x <- as.vector(x)
  code <- .cstr_apply(list(x, units = units), "grid::unit", ...)
  repair_attributes_simpleUnit(x, code, ...)
}

repair_attributes_simpleUnit <- function(x, ...) {
  .cstr_repair_attributes(x, idiomatic_class = c("simpleUnit", "unit", "unit_v2"), ignore = "unit", ...)
}

#' @export
.cstr_construct.margin <- function(x, ...) {
  lkp <- c(
    npc = 0L, cm = 1L, inches = 2L, mm = 7L, points = 8L, picas = 9L,
    bigpts = 10L, dida = 11L, cicero = 12L, scaledpts = 13L, lines = 3L,
    char = 18L, native = 4L, snpc = 6L, mylines = 103L, mychar = 104L,
    mystrwidth = 105L, mystrheight = 106L
  )
  unit <- names(lkp)[match(attr(x, "unit"), lkp)]
  x <- as.vector(x)
  code <- .cstr_apply(list(t = x[1], r = x[2], b = x[3], l = x[4], unit = unit), "ggplot2::margin", ...)
  repair_attributes_margin(x, code, ...)
}

repair_attributes_margin <- function(x, ...) {
  .cstr_repair_attributes(x, idiomatic_class = c("margin", "simpleUnit", "unit", "unit_v2"), ignore = "unit", ...)
}

#' @export
.cstr_construct.rel <- function(x, ...) {
  code <- .cstr_apply(list(unclass(x)), "ggplot2::rel", ...)
  repair_attributes_rel(x, code, ...)
}

repair_attributes_rel <- function(x, ...) {
  .cstr_repair_attributes(x, idiomatic_class = "rel", ...)
}
