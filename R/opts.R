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


#' Fetch constructive options
#'
#' Exported for custom constructor design.
#'
#' @param class A string. An S3 class.
#' @param ...,template Parameters generally forwarded through the dots of the caller function
#'
#' @return An object of class `c(paste0("constructive_options_", class), "constructive_options")`
#' @export
.cstr_fetch_opts <- function(class, ..., template = NULL) {
  options_class <- paste0("constructive_options_", class)
  # opts_funs defined .onLoad
  opts_from_dots <- rlang::with_bindings(list(...), !!!all_opts_funs, .env = .GlobalEnv)
  opts_from_dots <- Filter(function(x) inherits(x, options_class), opts_from_dots)
  if (length(opts_from_dots)) return(opts_from_dots[[1]])
  opts_from_template <- Filter(function(x) inherits(x, options_class), template)
  if (length(opts_from_template)) return(opts_from_template[[1]])
  match.fun(paste0("opts_", class))()
}

#' @export
print.constructive_options <- function(x, ...) {
  cl <- cli::col_blue(sprintf("<%s>", paste(class(x), collapse = "/")))
  opts <- vapply(x, .cstr_construct, character(1), one_liner = TRUE, template = NULL, data = NULL)
  # This assumes options are all scalar or NULL
  nms <- format(paste0(cli::col_blue(names(x)), ":"))
  writeLines(c(cl, paste(nms, opts)))
  invisible(x)
}
