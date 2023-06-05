
#' Register constructors
#'
#' Use this function to register a custom constructor. See vignette for more information.
#'
#' @param class A string
#' @param ... named constructors
#' @return Returns `NULL` invisibly, called for side effects.
#' @export
.cstr_register_constructors <- function(class, ...) {
  dots <- list(...)
  for (nm in names(dots)) {
    constructors[[class]][[nm]] <- dots[[nm]]
  }
  invisible(NULL)
}
