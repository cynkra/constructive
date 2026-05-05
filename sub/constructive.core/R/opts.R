collect_opts <- function(..., template) {
  opts_from_dots <- rlang::with_bindings(list(...), !!!all_opts_funs, .env = .GlobalEnv)
  opts <- c(opts_from_dots, template)
  names(opts) <- sapply(opts, function(x) sub("^constructive_options_(.*)$", "\\1", class(x)[[1]]))
  opts <- opts[unique(names(opts))]
  # inherit from atomic
  # logical
  opts$logical$constructor <-
    opts$logical$constructor %||%
    opts$atomic$constructor %||%
    "default"
  opts$logical$trim <-
    opts$logical$trim %||%
    opts$atomic$trim
  opts$logical$fill <-
    opts$logical$fill %||%
    opts$atomic$fill %||%
    "default"
  opts$logical$compress <-
    opts$logical$compress %||%
    opts$atomic$compress %||%
    TRUE
  # integer
  opts$integer$constructor <-
    opts$integer$constructor %||%
    opts$atomic$constructor %||%
    "default"
  opts$integer$trim <-
    opts$integer$trim %||%
    opts$atomic$trim
  opts$integer$fill <-
    opts$integer$fill %||%
    opts$atomic$fill %||%
    "default"
  opts$integer$compress <-
    opts$integer$compress %||%
    opts$atomic$compress %||%
    TRUE
  # double
  opts$double$constructor <-
    opts$double$constructor %||%
    opts$atomic$constructor %||%
    "default"
  opts$double$trim <-
    opts$double$trim %||%
    opts$atomic$trim
  opts$double$fill <-
    opts$double$fill %||%
    opts$atomic$fill %||%
    "default"
  opts$double$compress <-
    opts$double$compress %||%
    opts$atomic$compress %||%
    TRUE
  # complex
  opts$complex$constructor <-
    opts$complex$constructor %||%
    opts$atomic$constructor %||%
    "default"
  opts$complex$trim <-
    opts$complex$trim %||%
    opts$atomic$trim
  opts$complex$fill <-
    opts$complex$fill %||%
    opts$atomic$fill %||%
    "default"
  opts$complex$compress <-
    opts$complex$compress %||%
    opts$atomic$compress %||%
    TRUE
  # raw
  opts$raw$constructor <-
    opts$raw$constructor %||%
    opts$atomic$constructor %||%
    "as.raw"
  opts$raw$trim <-
    opts$raw$trim %||%
    opts$atomic$trim
  opts$raw$fill <-
    opts$raw$fill %||%
    opts$atomic$fill %||%
    "default"
  opts$raw$compress <-
    opts$raw$compress %||%
    opts$atomic$compress %||%
    TRUE
  opts$raw$representation <-
    opts$raw$representation %||%
    "hexadecimal"
  # character
  opts$character$constructor <-
    opts$character$constructor %||%
    opts$atomic$constructor %||%
    "default"
  opts$character$trim <-
    opts$character$trim %||%
    opts$atomic$trim
  opts$character$fill <-
    opts$character$fill %||%
    opts$atomic$fill %||%
    "default"
  opts$character$compress <-
    opts$character$compress %||%
    opts$atomic$compress %||%
    TRUE
  opts$character$unicode_representation <-
    opts$character$unicode_representation %||%
    opts$atomic$unicode_representation
  opts$character$escape <-
    opts$character$escape %||%
    opts$atomic$escape

  opts
}

#' Create constructive options
#'
#' Exported for custom constructor design.
#'
#' @param class A string. An S3 class.
#' @param ... Options to set
#'
#' @return An object of class `c(paste0("constructive_options_", class), "constructive_options")`
#' @export
.cstr_options <- function(class, ...) {
  structure(
    class = c(paste0("constructive_options_", class), "constructive_options"),
    list(...)
  )
}

#' @export
print.constructive_options <- function(x, ...) {
  cl <- cli::col_blue(sprintf("<%s>", paste(class(x), collapse = "/")))
  opts <- vapply(x, function(x, ...) .cstr_construct(x, ...), character(1), one_liner = TRUE, template = NULL, data = NULL, opts = NULL)
  # This assumes options are all scalar or NULL
  nms <- format(paste0(cli::col_blue(names(x)), ":"))
  writeLines(c(cl, paste(nms, opts)))
  invisible(x)
}
