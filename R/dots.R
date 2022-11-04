# note: waldo doesn't see the difference between dots

construct_idiomatic.dots <- function(x, ...) {
  quo_dots <- with(list(... = x), rlang::enquos(...))
  envs <- lapply(quo_dots, rlang::quo_get_env)
  unique_env <- unique(envs)
  if (length(unique_env) == 1) {
    unique_env <- unique_env[[1]]
    exprs <- lapply(quo_dots, rlang::quo_get_expr)
    code_lng <- rlang::expr((function(...) environment()$...)(!!!exprs))
    code <- deparse_call_impl(code_lng)
    env_code <- construct_raw(unique_env, ...)
    code <- construct_apply(list(code, envir = env_code), "evalq", language = TRUE)
    return(code)
  }
  # strip class since it's not necessary for splicing
  quo_code <- construct_raw(unclass(quo_dots), ...)
  quo_code[[1]] <- paste0("!!!", quo_code[[1]])
  code <- wrap(quo_code, "(function(...) environment()$...)")
  code <- wrap(code, "rlang::inject")
  code
}
