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
  CLASS_CHR <- as.character(CLASS)
  IS_CORRUPTED_FUN <- as.symbol(paste0("is_corrupted_", class))
  CONSTRUCTOR_ARGS <- sapply(constructors, as.symbol)
  DOTS <- eval(substitute(alist(...)))
  FORWARDED_DOTS <- DOTS
  FORWARDED_DOTS[] <- lapply(names(DOTS), function(x) call("$", quote(opts), as.symbol(x)))
  OPTS_FUN = as.symbol(sprintf("opts_%s", CLASS_CHR))
  eval(bquote(
    splice = TRUE,
    as.function(
    alist(x = , opts =, ... = ,{
      opts_local <- opts[[.(CLASS_CHR)]] %||% .(OPTS_FUN)()
      if (.(IS_CORRUPTED_FUN)(x) || opts_local[["constructor"]] == "next") return(NextMethod())
      constructor <- constructors[[.(CLASS)]][[opts_local[["constructor"]]]]
      constructor(x, opts = opts, ..(FORWARDED_DOTS), ...)
    }),
    envir = env
    )
  ))
}
