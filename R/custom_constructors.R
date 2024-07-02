#' Extend constructive
#'
#' @description
#' We export a collection of functions that can be used to design custom methods for
#' \link{.cstr_construct}() or custom constructors for a given method.
#'
#' * \link{.cstr_new_class}() : Open template to support a new class
#' * \link{.cstr_new_constructor}() : Open template to implement a new constructor
#' * \link{.cstr_construct}() : Low level generic for object construction code generation
#' * \link{.cstr_repair_attributes}()` : Helper to repair attributes of objects
#' * \link{.cstr_options}() : Define and check options to pass to custom constructors
#' * \link{.cstr_apply}() : Build recursively the arguments passed to your constructor
#' * \link{.cstr_wrap}() : Wrap argument code in function code (rarely needed)
#' * \link{.cstr_pipe}() : Pipe a call to another (rarely needed)
#' * \link{.cstr_combine_errors}() : helper function report several errors
#'  at once when relevant
#' @name extend-constructive
NULL
