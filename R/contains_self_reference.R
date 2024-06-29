
contains_self_reference <- function(x, envs = character()) {
  if (is.null(x)) return(FALSE)
  if (is.environment(x)) {
    if (!is.null(construct_special_env(x))) return(FALSE)
    address <- rlang::obj_address(x)
    if (address %in% envs) return(TRUE)
    envs <- c(envs, address)
    bindings <- names(x)
    lazy_bindings <- bindings[rlang::env_binding_are_lazy(x)]
    lazy_binding_envs <- lapply(lazy_bindings, promise_env, x)
    for (lazy_binding_env in lazy_binding_envs) {
      if (contains_self_reference(lazy_binding_env, envs)) return(TRUE)
    }
    bindings <- setdiff(bindings, lazy_bindings)
    for (binding in bindings) {
      obj <- get(binding, x)
      if (contains_self_reference(obj, envs)) return(TRUE)
    }
    if (contains_self_reference(parent.env(x), envs)) return(TRUE)
  } else if (is.list(x)) {
    for (elt in x) {
      if (contains_self_reference(elt, envs)) return(TRUE)
    }
  } else if (is.function(x)) {
    if (contains_self_reference(environment(x), envs)) return(TRUE)
  }
  # this correctly uses the updated envs if x is an environment
  if (contains_self_reference(attributes(x), envs)) return(TRUE)
  FALSE
}
