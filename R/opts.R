
fetch_opts <- function(suffix, ..., template) {
  class <- paste0("constructive_options_", suffix)
  opts_from_dots <- Filter(function(x) inherits(x, class) , list(...))
  if (length(opts_from_dots)) return(opts_from_dots[[1]])
  opts_from_template <- Filter(function(x) inherits(x, class) , template)
  if (length(opts_from_template)) return(opts_from_template[[1]])
  match.fun(paste0("opts_", suffix))()
}

#' @export
print.constructive_options <- function(x, ...) {
  cl <- cli::col_blue(sprintf("<%s>", paste(class(x), collapse = "/")))
  opts <- vapply(x, construct_raw, character(1), one_liner = TRUE, template = NULL)
  # This assumes options are all scalar or NULL
  nms <- format(paste0(cli::col_blue(names(x)), ":"))
  writeLines(c(cl, paste(nms, opts)))
  invisible(x)
}
