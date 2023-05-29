# maybe we should use "..." because we might have objects of class "dots"
constructors$dots <- new.env()

#' Constructive options for type '...'
#'
#' These options will be used on objects of type '...'. These are rarely encountered
#' in practice. By default this function is useless as nothing can be set, this
#' is provided in case users want to extend the method with other constructors.
#'
#'
#' Depending on `constructor`, we construct the environment as follows:
#' * `"default"` : We use the construct `(function(...) environment()$...)(a = x, y)`
#'   which we evaluate in the correct environment.
#'
#' @param constructor String. Name of the function used to construct the environment.
#' @inheritParams opts_atomic
#'
#' @return An object of class <constructive_options/constructive_options_environment>
#' @export
opts_dots <- function(constructor = c("default"), ...) {
  .cstr_combine_errors(
    constructor <- .cstr_match_constructor(constructor, "dots"),
    ellipsis::check_dots_empty()
  )
  .cstr_options("dots", constructor = constructor)
}

#' @export
.cstr_construct.dots <- function(x, ...) {
  opts <- .cstr_fetch_opts("dots", ...)
  if (is_corrupted_dots(x)) return(NextMethod())
  constructor <- constructors$dots[[opts$constructor]]
  constructor(x, ...)
}

is_corrupted_dots <- function(x) {
  typeof(x) != "..."
}

constructors$dots$default <- function(x, ...) {
  quo_dots <- with(list(... = x), rlang::enquos(...))
  envs <- lapply(quo_dots, rlang::quo_get_env)
  unique_env <- unique(envs)
  if (length(unique_env) == 1) {
    unique_env <- unique_env[[1]]
    exprs <- lapply(quo_dots, rlang::quo_get_expr)
    code_lng <- rlang::expr((function(...) environment()$...)(!!!exprs))
    code <- deparse_call(code_lng, style = FALSE)
    env_code <- .cstr_construct(unique_env, ...)
    code <- .cstr_apply(list(code, envir = env_code), "evalq", recurse = FALSE)
    return(code)
  }
  # strip class since it's not necessary for splicing
  quo_code <- .cstr_construct(unclass(quo_dots), ...)
  quo_code[[1]] <- paste0("!!!", quo_code[[1]])
  code <- .cstr_wrap(quo_code, "(function(...) environment()$...)")
  code <- .cstr_wrap(code, "rlang::inject")

  repair_attributes_dots(x, code, ...)
}

repair_attributes_dots <- function(x, code, ...) {
  # FIXME: add a repair fun since dots can have attributes, come back after environments are done
  code
}
