#' Constructive options for type '...'
#'
#' These options will be used on objects of type '...'. These are rarely encountered
#' in practice. By default this function is useless as nothing can be set, this
#' is provided in case users want to extend the method with other constructors.
#'
#'
#' Depending on `constructor`, we construct the object as follows:
#' * `"default"` : We use the construct `(function(...) get(\"...\"))(a = x, y)`
#'   which we evaluate in the correct environment.
#'
#' @param constructor String. Name of the function used to construct the object.
#' @inheritParams opts_atomic
#'
#' @return An object of class <constructive_options/constructive_options_dots>
#' @export
opts_dots <- function(constructor = c("default"), ...) {
  .cstr_options("dots", constructor = constructor[[1]], ...)
}

#' @export
#' @method .cstr_construct dots
.cstr_construct.dots <- function(x, ...) {
  opts <- list(...)$opts$dots %||% opts_dots()
  if (is_corrupted_dots(x)) return(NextMethod())
  UseMethod(".cstr_construct.dots", structure(NA, class = opts$constructor))
}

is_corrupted_dots <- function(x) {
  typeof(x) != "..."
}

#' @export
#' @method .cstr_construct.dots default
.cstr_construct.dots.default <- function(x, ...) {
  quo_dots <- with(list(... = x), rlang::enquos(...))
  envs <- lapply(quo_dots, rlang::quo_get_env)
  unique_env <- unique(envs)
  if (length(unique_env) == 1) {
    unique_env <- unique_env[[1]]
    exprs <- lapply(quo_dots, rlang::quo_get_expr)
    code_lng <- rlang::expr((function(...) get("..."))(!!!exprs))
    code <- deparse_call0(code_lng, ...)
    env_code <- .cstr_construct(unique_env, ...)
    code <- .cstr_apply(list(code, envir = env_code), "evalq", recurse = FALSE)
    return(repair_attributes_dots(x, code, ...))
  }
  # strip class since it's not necessary for splicing
  quo_code <- .cstr_construct(unclass(quo_dots), ...)
  quo_code[[1]] <- paste0("!!!", quo_code[[1]])
  code <- .cstr_wrap(quo_code, "(function(...) get(\"...\"))")
  code <- .cstr_wrap(code, "rlang::inject")

  repair_attributes_dots(x, code, ...)
}

repair_attributes_dots <- function(x, code, ...) {
  .cstr_repair_attributes(x, code, ...)
}
