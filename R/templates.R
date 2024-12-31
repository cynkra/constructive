#' Extend constructive
#'
#' @description
#' `.cstr_new_class()` and `.cstr_new_constructor()` open new unsaved scripts,
#' optionally commented, that can be used as templates to define new constructors.
#' If the class is already supported and you want to implement a new constructor,
#' use `.cstr_new_constructor()`, otherwise use `.cstr_new_class()`.
#'
#' @details
#'
#' We suggest the following workflow (summarized in a message when you call the functions):
#' * Call `usethis::use_package(\"constructive\"`, \"Suggests\")` one time at any
#'   point, this will add a soft dependency on 'constructive' so it's only needed to
#'   install it when you use it.
#' * Call `.cstr_new_class()` or `.cstr_new_constructor()`, with `commented = TRUE` for more guidance.
#' * Save the scripts unchanged in the "R" folder of your package.
#' * `devtools::document()`: this will register the S3 methods.
#' * Try `construct()` on your new object, it should print a call to your chosen
#'   constructor.
#' * Tweak the code, in particular the definition of `args`.
#'
#' The README of the example extension package
#' ['constructive.example'](https://github.com/cynkra/constructive.example)
#' guides you through the process. See also \{constructive\}'s own code
#' and `vignette("extend-constructive")` for more details.
#'
#' @param class Class to support, provide the full `class()` vector.
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
    constructor = "PKG::CONSTRUCTOR",
    commented = FALSE) {
  template <- if (commented) {
    system.file("new_class_template_commented_no_import.R", package = "constructive")
  } else {
    system.file("new_class_template_no_import.R", package = "constructive")
  }
  code <- readLines(template)
  code <- gsub(".CLASS.", .cstr_construct(class, one_liner = TRUE), code, fixed = TRUE)
  code <- gsub(".CLASS1.", class[[1]], code, fixed = TRUE)
  code <- gsub(".PKG::CONSTRUCTOR.", constructor, code, fixed = TRUE)
  code <- gsub(".CONSTRUCTOR.", sub("^.*::(.*)$", "\\1", constructor), code, fixed = TRUE)
  rstudioapi::documentNew(code)
  inform(c(
    `*` = "Call `usethis::use_package(\"constructive\"`, \"Suggests\")`",
    `*` = "Save the script in your package's R folder",
    `*` = "Call `devtools::document()`",
    `*` = "Tweak and iterate"
  ))
  invisible(NULL)
}

#' @rdname templates
#' @export
.cstr_new_constructor <- function(class = c("CLASS", "PARENT_CLASS"), constructor = "PKG::CONSTRUCTOR", commented = FALSE) {
  template <- if (commented) {
    system.file("new_constructor_template_commented_no_import.R", package = "constructive")
  } else {
    system.file("new_constructor_template_no_import.R", package = "constructive")
  }
  code <- readLines(template)
  code <- gsub(".CLASS.", .cstr_construct(class, one_liner = TRUE), code, fixed = TRUE)
  code <- gsub(".CLASS1.", class[[1]], code, fixed = TRUE)
  code <- gsub(".PKG::CONSTRUCTOR.", constructor, code, fixed = TRUE)
  code <- gsub(".CONSTRUCTOR.", sub("^.*::(.*)$", "\\1", constructor), code, fixed = TRUE)
  rstudioapi::documentNew(code)
  inform(c(
    `*` = "Call `usethis::use_package(\"constructive\"`, \"Suggests\")`",
    `*` = "Save the script in your package's R folder",
    `*` = "Call `devtools::document()`",
    `*` = "Tweak and iterate"
  ))
  invisible(NULL)
}
