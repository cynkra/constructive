
construct_special_env <- function(x) {
  if (identical(x, baseenv())) return("baseenv()")
  if (identical(x, emptyenv())) return("emptyenv()")
  if (identical(x, .GlobalEnv)) return(".GlobalEnv")
  if (identical(x, .BaseNamespaceEnv)) return(".BaseNamespaceEnv")
  # testing on name is not enough but we use it to identify candidated
  name <- environmentName(x)
  # handle {testthat} corner case
  if (identical(Sys.getenv("TESTTHAT"), "true") && name == "constructive") return('asNamespace("constructive")')
  if (name != "" && rlang::is_installed(name) && identical(x, asNamespace(name))) return(sprintf('asNamespace("%s")', name))
  if (name %in% search() && identical(x, as.environment(name))) return(sprintf('as.environment("%s")', name))
}

env_memory_address <- function(x, by_name = FALSE) {
  if (identical(Sys.getenv("TESTTHAT"), "true")) return("0x123456789")
  if (by_name) rlang::env_label(x) else rlang::obj_address(x)
}

# adapted from rlang::env_name
env_name <- function (env) {
  if (identical(env, global_env())) {
    return("global")
  }
  if (identical(env, base_env())) {
    return("package:base")
  }
  if (identical(env, empty_env())) {
    return("empty")
  }
  nm <- environmentName(env)
  if (isNamespace(env)) {
    return(paste0("namespace:", nm))
  }
  nm
}

fetch_parent_names <- function(x) {
  parents <- character()
  repeat {
    x <- parent.env(x)
    # An environment should always have a parent but for some reason some have
    # a NULL parent, though the error of `new.env(parent = NULL)` says the feature
    # is defunct
    if (is.null(x)) return(parents)
    nm <- env_name(x)
    if (nm != "") {
      return(c(parents, nm))
    }
    nm <- env_memory_address(x, by_name = TRUE)
    parents <- c(parents, nm)
  }
}

#' Fetch environment from memory address
#'
#' This is designed to be used in constructed output. The `parents` and `...` arguments
#'  are not processed and only used to display additional information. If used on
#'  an improper memory address the output might be erratic or the session might crash.
#'
#' @param address Memory address of the environment
#' @param parents,... ignored
#' @return The environment that the memory address points to.
#' @export
.env <- function(address, parents = NULL, ...) {
  force(parents) # to avoid notes
  env <- env_impl(address)
  if (is.null(env)) {
    msg <- sprintf("No environment was found at the memory address '%s'", address)
    info1 <- "It's likely that {constructive} was called in a different session to generate this code."
    info2 <- "The environment might also have been garbage collected."
    info3 <- "See `?opts_environment` for various alternatives to construct environment with persistent definitions."
    abort(c(msg, i = info1, i = info2, i = info3))
  }
  env
}

update_predefinition <- function(envir, ...) {
  # construct parent before constructing env
  parent_code <- .cstr_construct(parent.env(envir), ...)
  # if envir was already constructed (recorded in globals$env), just return its name
  hash <- format(envir)
  if (hash %in% globals$envs$hash) {
    return(globals$envs$name[hash == globals$envs$hash][[1]])
  }
  # create a new name, incrementing count
  env_name <- sprintf("..env.%s..", nrow(globals$envs) + 1)
  # update globals$envs with new row (hash + variable name)
  globals$envs <- rbind(globals$envs, data.frame(hash = hash, name = env_name))
  # initialize with new.env(), repairing attributes
  code <- sprintf("new.env(parent = %s)", parent_code)
  # hack: we use  opts_environment(predefine = FALSE) to switch on attribute reparation just there
  code <- repair_attributes_environment(envir, code, opts_environment(predefine = FALSE), ...)
  code[[1]] <- sprintf("%s <- %s", env_name, code[[1]])
  # update predefinitions with envir definition
  globals$predefinition <- c(
    globals$predefinition,
    code
  )
  # build non environment objects of envir above
  for (nm in names(envir)) {
    obj <- envir[[nm]]
    if (missing(obj)) {
      obj_code <- sprintf("%s$%s <- quote(expr = )", env_name, nm)
      globals$predefinition <- c(
        globals$predefinition,
        obj_code
      )
    } else if (!is.environment(obj)) {
      nm <- protect(nm)
      # this defines the objects as a side effect
      obj_code <- .cstr_construct(obj, ...)
      obj_code[[1]] <- sprintf("%s$%s <- %s", env_name, nm, obj_code[[1]])
      globals$predefinition <- c(
        globals$predefinition,
        obj_code
      )
    }
  }

  # build environment objects of envir above
  for (nm in names(envir)) {
    obj <- envir[[nm]]
    if (missing(obj)) {
      obj_code <- sprintf("%s$%s <- quote(expr = )", env_name, nm)
      globals$predefinition <- c(
        globals$predefinition,
        obj_code
      )
    } else if (is.environment(obj)) {
      nm <- protect(nm)
      sub_env_name <- .cstr_construct(obj, ...) # this will also print code
      globals$predefinition <- c(
        globals$predefinition,
        sprintf("%s$%s <- %s", env_name, nm, sub_env_name)
      )
    }
  }
  env_name
}
