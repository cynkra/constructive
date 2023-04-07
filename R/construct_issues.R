#' Show constructive issues
#'
#' @param x An object built by `construct()`, if `NULL` the latest encountered
#'   issues will be displayed
#'
#' @return A character vector with class "waldo_compare"
#' @export
construct_issues <- function(x = NULL) {
  if (is.null(x)) return(globals$issues) # nocov
  x$compare
}
