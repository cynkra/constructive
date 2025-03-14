# FIXME: instead of returning TRUE we could keep tract of nesting like waldo
#   does and return the location of the problematic env
contains_self_reference <- function(
    x,
    envs = character(),
    check_parent = TRUE,
    check_function_env = TRUE,
    check_srcref = FALSE
    ) {
  rec <- function(x) {
    contains_self_reference(x, envs, check_parent, check_function_env, check_srcref)
  }
  if (is.null(x)) return(FALSE)
  if (is.environment(x)) {
    if (!is.null(construct_special_env(x))) return(FALSE)
    address <- rlang::obj_address(x)
    if (address %in% envs) return(TRUE)
    envs <- c(envs, address)
    # since we override S3 dispatch here we can circumvent rlang bug
    # https://github.com/r-lib/rlang/issues/1783
    bindings <- names(x)
    lazy_bindings <- bindings[rlang::env_binding_are_lazy(x, bindings)]
    lazy_binding_envs <- lapply(lazy_bindings, promise_env, x)
    for (lazy_binding_env in lazy_binding_envs) {
      if (rec(lazy_binding_env)) return(TRUE)
    }
    bindings <- setdiff(bindings, lazy_bindings)
    for (binding in bindings) {
      obj <- get(binding, x)
      if (rec(obj)) return(TRUE)
    }
    if (check_parent && rec(parent.env(x))) return(TRUE)
  } else if (is.list(x)) {
    for (elt in x) {
      if (rec(elt)) return(TRUE)
    }
  } else if (is.function(x)) {
    if (check_function_env && rec(environment(x))) return(TRUE)
    if (!check_srcref) attr(x, "srcref") <- NULL
  }
  # this correctly uses the updated envs if x is an environment

  if (rec(names(x))) return(TRUE)
  attrs <- attributes(x)
  attrs$names <- NULL
  # to avoid infinite recursion we don't inspect names
  # names can be only characters so there
  if (length(attrs) && rec(attrs)) return(TRUE)
  FALSE
}
