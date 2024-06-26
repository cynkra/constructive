#' Extend constructive
#'
#' @description
#' `.cstr_new_class()` and `.cstr_new_constructor()` open new unsaved scripts,
#' optionally commented, that can be used as templates to define new constructors.
#' If the class is already supported and you want to implement a new constructor,
#' use `.cstr_new_constructor()`, otherwise use `.cstr_new_class()`.
#'
#' We recommend working from these templates and examples in this package or
#'
#'
#' @details
#'
#' We suggest the following workflow :
#' * Call these functions, with `commented = TRUE` for more guidance
#' * Save the scripts unchanged in your package
#' * `devtools::document()`: this will register the S3 methods
#' * Try `construct()` on your new object, it should print a call to your chosen
#'   constructor
#' * Tweak the code, in particular the definition of `args`
#'
#' For more information check `vignette("extend-constructive")`, the example
#' extension package
#' ['constructive.example'](https://github.com/cynkra/constructive.example)
#' or \{constructive\}'s own code.
#'
#' @param class Class to support, for `.cstr_new_class()` provide the full
#'   `class()` vector, for `.cstr_new_constructor()` the first element is enough.
#' @param constructor Name of the constructor, usually the name of the function
#'   you can to use to build the object. If not you might need to adjust the
#'   script.
#' @param commented Boolean. Whether to include comments in the template.
#'
#' @return Both function return `NULL` invisibly and are called for side effects
#' @name templates
#' @export
.cstr_new_class <- function(
    class = c("CLASS", "PARENT_CLASS"),
    constructor = "CONSTRUCTOR",
    commented = FALSE) {
  template <- if (commented) {
    system.file("new_class_template_commented.R", package = "constructive")
  } else {
    system.file("new_class_template.R", package = "constructive")
  }
  code <- readLines(template)
  code <- gsub("CLASS1", class[[1]], code, fixed = TRUE)
  code <- gsub(
    "idiomatic_class = CLASS",
    sprintf("idiomatic_class = %s", .cstr_construct(class, one_liner = TRUE)),
    code,
    fixed = TRUE)
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
  code <- gsub("CLASS1", class[[1]], code, fixed = TRUE)
  code <- gsub("CONSTRUCTOR", constructor, code, fixed = TRUE)
  rstudioapi::documentNew(code)
  invisible(NULL)
}
