register_constructors <- function(class, ...) {
  if (!requireNamespace("constructive")) return(invisible(NULL))
  constructors <- list(...)
  nms <- names(constructors)
  ns <- asNamespace("constructive")
  unlockBinding("constructors", ns)
  if (exists(class, ns$constructors)) {
    opts_fun_name <- paste0("opts_", class)
    old_constructors <- eval(formals(ns[[opts_fun_name]])[[1]])
    nms_new <- setdiff(nms, old_constructors)
    next_pos <- match("next", old_constructors)
    if (is.na(next_pos)) {
      new_constructors <- c(old_constructors, nms_new)
    } else {
      new_constructors <- c(
        old_constructors[seq_len(next_pos-1)],
        nms_new,
        old_constructors[seq(next_pos, length(old_constructors))]
      )
    }
    unlockBinding(opts_fun_name, env = ns)
    formals(ns[[opts_fun_name]])[[1]] <- new_constructors
    lockBinding(opts_fun_name, env = ns)
    if ("package:constructive" %in% search()) {
      pkg_env <- as.environment("package:constructive")
      unlockBinding(opts_fun_name, env = pkg_env)
      formals(pkg_env[[opts_fun_name]])[[1]] <- new_constructors
      lockBinding(opts_fun_name, env = pkg_env)
    }
  } else {
    ns$constructors[[class]] <- new.env()
  }
  # assign new constructors, warn if overriding is attempted
  for (i in seq_along(constructors)) {
    if (nms[[i]] %in% ls(ns$constructors[[class]], all.names = TRUE)) {
      rlang::warn(sprintf("'%s' is already defined as a constructor for class '%s' and won't be overriden.", nms[[i]], class))
      next
    }
    ns$constructors[[class]][[nms[[i]]]] <- constructors[[i]]
  }
  lockBinding("constructors", ns)
  invisible(NULL)
}
