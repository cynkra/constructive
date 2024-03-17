# functions used to build functions of the package
new_constructive_opts_function <- function(class, constructors, ...) {

  env <- parent.frame()
  DOTS <- eval(substitute(alist(...)))
  CONSTRUCTOR <- substitute(constructors)
  FORWARDED_DOTS <- DOTS
  FORWARDED_DOTS[] <- lapply(names(DOTS), as.symbol)
  CLASS <- substitute(class)
  eval(bquote(
    splice = TRUE,
    as.function(
    alist(constructor = .(CONSTRUCTOR), ... =, ..(DOTS), {
      .cstr_combine_errors(
        constructor <- .cstr_match_constructor(constructor, .(CLASS)),
        check_dots_empty()
      )
      .cstr_options(.(CLASS), constructor = constructor, ..(FORWARDED_DOTS))
    }),
    envir = env
    )
  ))
}

new_constructive_method <- function(class, constructors, ...) {
  env <- parent.frame()
  CLASS <- substitute(class)
  IS_CORRUPTED_FUN <- as.symbol(paste0("is_corrupted_", class))
  CONSTRUCTOR_ARGS <- sapply(constructors, as.symbol)
  DOTS <- eval(substitute(alist(...)))
  FORWARDED_DOTS <- DOTS
  FORWARDED_DOTS[] <- lapply(names(DOTS), function(x) call("$", quote(opts), as.symbol(x)))
  eval(bquote(
    splice = TRUE,
    as.function(
    alist(x = , ... = ,{
      opts <- .cstr_fetch_opts(.(CLASS), ...)
      if (.(IS_CORRUPTED_FUN)(x) || opts$constructor == "next") return(NextMethod())
      constructor <- constructors[[.(CLASS)]][[opts$constructor]]
      constructor(x, ..(FORWARDED_DOTS), ...)
    }),
    envir = env
    )
  ))
}
