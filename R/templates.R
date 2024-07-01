#' Extend constructive
#'
#' `.cstr_new_class()` and `.cstr_new_constructor()` open new unsaved scripts,
#' optionally commented, that can be used as templates to define new constructors.
#' If the class is already supported and you want to implement a new constructor,
#' use `.cstr_new_constructor()`, otherwise use `.cstr_new_class()`.
#'
#' We recommend working from these templates and examples in this package or
#' the example extension package ['constructive.example'](https://github.com/cynkra/constructive.example)
#'
#' @param class Name of the class
#' @param constructor Name of the constructor
#' @param commented Boolean. Whether to include comments in the template.
#'
#' @return Both function return `NULL` invisibly and are called for side effects
#' @name templates
#' @export
.cstr_new_class <- function(class = "CLASS", constructor = "CONSTRUCTOR", commented = FALSE) {
  template <- if (commented) {
    system.file("new_class_template_commented.R", package = "constructive")
  } else {
    system.file("new_class_template.R", package = "constructive")
  }
  code <- readLines(template)
  code <- gsub("CLASS", class, code, fixed = TRUE)
  code <- gsub("CONSTRUCTOR", constructor, code, fixed = TRUE)
  rstudioapi::documentNew(code)
  invisible(NULL)
}

#' @rdname templates
#' @export
.cstr_new_constructor <- function(class = "CLASS", constructor = "CONSTRUCTOR", commented = FALSE) {
  template <- if (commented) {
    system.file("new_constructor_template_commented.R", package = "constructive")
  } else {
    system.file("new_constructor_template.R", package = "constructive")
  }
  code <- readLines(template)
  code <- gsub("CLASS", class, code, fixed = TRUE)
  code <- gsub("CONSTRUCTOR", constructor, code, fixed = TRUE)
  rstudioapi::documentNew(code)
  invisible(NULL)
}
