#' Dump Constructed Code to a File
#'
#' An alternative to `base::dump()` using code built with \pkg{constructive}.
#'
#' @param x A named list or an environment.
#' @param path File or connection to write to.
#' @param append If FALSE, will overwrite existing file. If TRUE, will append to existing file. In both cases, if the file does not exist a new file is created.
#' @param ... Forwarded to `construct_multi()`
#'
#' @return Returns `NULL` invisibly, called for side effects.
#' @export
construct_dump <- function(x, path, append = FALSE, ...) {
  .cstr_combine_errors(
    x,
    abort_not_string(path)
  )
  constructive <- construct_multi(x, ...)
  cat(constructive$code, file = path, sep = "\n", append = append)
  invisible(NULL)
}
