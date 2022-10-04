
fetch_opts <- function(suffix, ...) {
  class <- paste0("constructive_options_", suffix)
  opts <- Filter(function(x) inherits(x, class) , list(...))
  if (length(opts)) opts[[1]] else match.fun(paste0("opts_", suffix))()
}

#' @export
print.constructive_options <- function(x, ...) {
  cl <- cli::col_blue(sprintf("<%s>", paste(class(x), collapse = "/")))
  opts <- vapply(x, construct_raw, character(1), one_liner = TRUE)
  # This assumes options are all scalar or NULL
  nms <- format(paste0(cli::col_blue(names(x)), ":"))
  writeLines(c(cl, paste(nms, opts)))
  invisible(x)
}

# opts_function <- function(trim = NULL, as.function = FALSE, zap_srcref = FALSE, construct_env = FALSE) {
#   # insert check and processing of args here
#   structure(
#     class = c("constructive_options", "constructive_options_function"),
#     list(
#       trim = trim,
#       as.function = as.function,
#       zap_srcref = zap_srcref,
#       construct_env = construct_env
#     )
#   )
# }
