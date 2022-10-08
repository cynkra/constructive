#' Constructive options for type 'list'
#'
#' These options will be used on objects of type 'list'.
#'
#' Depending on `constructor`, we construct the environment as follows:
#' * `"list"` (default): Build the object by calling `list()`.
#' * `"list2"`: Build the object by calling `rlang::list2()`, the only difference with
#'   the above is that we keep a trailing comma when the list is not trimmed and the call
#'   spans several lines.
#'
#' If `trim` is provided, depending on `fill` we will present trimmed elements as followed:
#' * `"vector"` (default): Use `vector()`, so for instance `list("a", "b", "c")` might become `c(list("a"), vector("list", 2))`.
#' * `"new_list"`: Use `rlang::new_list()`, so for instance `list("a", "b", "c")` might become `c(list("a"), rlang::new_list(2))`.
#' * `"+"`: Use unary `+`, so for instance `list("a", "b", "c")` might become `list("a", +2)`.
#' * `"..."`: Use `...`, so for instance `list("a", "b", "c")` might `list("a", ...)`
#'
#' When `trim` is used the output is parsable but might not be possible to evaluate,
#' especially with `fill = "..."`. In that case you might want to set `check = FALSE`
#'
#' @param constructor String. Name of the function used to construct the environment, see Details section.
#' @param trim `NULL` or integerish. Maximum of elements showed before it's trimmed,
#' replacing code with `...`. Note that it will necessarily produce code that doesn't
#' reproduce the input. This code will parse without failure but its evaluation might fail.
#'
#' @return An object of class <constructive_options/constructive_options_list>
#' @export
opts_list <- function(
    constructor = c("list", "list2"),
    trim = NULL,
    fill = c("vector", "new_list", "+", "...", "none")) {
  combine_errors(
    constructor <- rlang::arg_match(constructor),
    abort_not_null_or_integerish(trim),
    fill <- rlang::arg_match(fill)
  )
  structure(
    class = c("constructive_options", "constructive_options_list"),
    list(constructor = constructor, trim = trim, fill = fill)
  )
}

#' @export
construct_idiomatic.list <- function(x, ...) {
  args <- fetch_opts("list", ...)
  trim <- args$trim
  fill <- args$fill
  if (!is.null(trim)) {
    l <- length(x)
    if (l > trim) {
      new_args <- lapply(x[seq_len(args$trim)], construct_raw, ...)
      if (fill %in% c("+", "...", "none")) {
        if (fill == "+") {
          new_args <- c(new_args, list(paste0("+", l - trim)))
        } else if (fill == "...") {
          new_args <- c(new_args, "...")
        }
        debugonce(construct_apply)
        code <- construct_apply(new_args, "list", ..., new_line = FALSE, language = TRUE)
        return(code)
      }

      list_code <- construct_apply(new_args, "list", ..., new_line = FALSE, language = TRUE)
      if (fill == "vector") {
        null_list_code <- sprintf('vector("list", %s)', l - trim)
      } else {
        # fill == "new_list
        null_list_code <- sprintf('rlang::new_list(%s)', l - trim)
      }
      code <- construct_apply(list(list_code, null_list_code), "c", ..., new_line = FALSE, language = TRUE)
      return(code)
    }
  }
  constructor <- args$constructor
  if (constructor == "list2") constructor <- "rlang::list2"
  construct_apply(x, fun = constructor, ..., keep_trailing_comma = constructor == "rlang::list2")
}
