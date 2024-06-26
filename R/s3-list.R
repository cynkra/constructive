#' Constructive options for type 'list'
#'
#' These options will be used on objects of type 'list'.
#'
#' Depending on `constructor`, we construct the object as follows:
#' * `"list"` (default): Build the object by calling `list()`.
#' * `"list2"`: Build the object by calling `rlang::list2()`, the only difference with
#'   the above is that we keep a trailing comma when the list is not trimmed and the call
#'   spans several lines.
#'
#' If `trim` is provided, depending on `fill` we will present trimmed elements as followed:
#' * `"vector"` (default): Use `vector()`, so for instance `list("a", "b", "c")` might become `c(list("a"), vector("list", 2))`.
#' * `"new_list"`: Use `rlang::new_list()`, so for instance `list("a", "b", "c")` might become `c(list("a"), rlang::new_list(2))`.
#' * `"+"`: Use unary `+`, so for instance `list("a", "b", "c")` might become `list("a", +2)`.
#' * `"..."`: Use `...`, so for instance `list("a", "b", "c")` might become `list("a", ...)`
#' * `"none"`: Don't represent trimmed elements.
#'
#' When `trim` is used the output is parsable but might not be possible to evaluate,
#' especially with `fill = "..."`. In that case you might want to set `check = FALSE`
#'
#' @param constructor String. Name of the function used to construct the object, see Details section.
#' @inheritParams opts_atomic
#' @param trim `NULL` or integerish. Maximum of elements showed before it's trimmed.
#' Note that it will necessarily produce code that doesn't reproduce the input.
#' This code will parse without failure but its evaluation might fail.
#' @param fill String. Method to use to represent the trimmed elements.
#'
#' @return An object of class <constructive_options/constructive_options_list>
#' @export
opts_list <- function(
    constructor = c("list", "list2"),
    ...,
    trim = NULL,
    fill = c("vector", "new_list", "+", "...", "none")) {
  .cstr_combine_errors(
    abort_not_null_or_integerish(trim),
    fill <- rlang::arg_match(fill)
  )
  .cstr_options("list", constructor = constructor[[1]], ..., trim = trim, fill = fill)
}

#' @export
#' @method .cstr_construct list
.cstr_construct.list <- function(x, ...) {
  opts <- list(...)$opts$list %||% opts_list()
  if (is_corrupted_list(x)) return(NextMethod())
  UseMethod(".cstr_construct.list", structure(NA, class = opts$constructor))
}

is_corrupted_list <- function(x) {
  typeof(x) != "list"
}

construct_list <- function(x, constructor, trim, fill, trailing_comma, ...) {
  if (!is.null(trim)) {
    l <- length(x)
    if (l > trim) {
      args <- lapply(x[seq_len(trim)], .cstr_construct, ...)
      if (fill %in% c("+", "...", "none")) {
        if (fill == "+") {
          args <- c(args, list(paste0("+", l - trim)))
        } else if (fill == "...") {
          args <- c(args, "...")
        }
        code <- .cstr_apply(args, constructor, ..., new_line = FALSE, recurse = FALSE, trailing_comma  = trailing_comma)
        return(code)
      }

      list_code <- .cstr_apply(args, constructor, ..., new_line = FALSE, recurse = FALSE, trailing_comma  = trailing_comma)
      if (fill == "vector") {
        null_list_code <- sprintf('vector("list", %s)', l - trim)
      } else {
        # fill == "new_list
        null_list_code <- sprintf("rlang::new_list(%s)", l - trim)
      }
      # args are not named here so no precaution needed for names args to c
      code <- .cstr_apply(list(list_code, null_list_code), "c", ..., new_line = FALSE, recurse = FALSE)
      return(code)
    }
  }
  nms <- names(x)
  repair_names <- names_need_repair(nms, c_names = FALSE)
  if (repair_names) names(x) <- NULL
  .cstr_apply(x, fun = constructor, ..., trailing_comma = trailing_comma)
}

#' @export
#' @method .cstr_construct.list list
.cstr_construct.list.list <- function(x, ...) {
  opts <- list(...)$opts$list %||% opts_list()
  code <- construct_list(x, "list", opts$trim, opts$fill, trailing_comma = FALSE, ...)
  repair_attributes_list(x, code, ...)
}

#' @export
#' @method .cstr_construct.list list2
.cstr_construct.list.list2 <- function(x, ...) {
  opts <- list(...)$opts$list %||% opts_list()
  code <- construct_list(x, "rlang::list2", opts$trim, opts$fill, trailing_comma = TRUE, ...)
  repair_attributes_list(x, code, ...)
}

repair_attributes_list <- function(x, code, ...) {
  nms <- names(x)
  repair_names <- !is.null(nms) && (anyNA(nms) || all(nms == ""))
  .cstr_repair_attributes(x, code, ..., repair_names = repair_names)
}
