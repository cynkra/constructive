# copied from vctrs, appropriately as described in ?vctrs::s3_register
s3_register <- function (generic, class, method = NULL) {
  stopifnot(is.character(generic), length(generic) == 1)
  stopifnot(is.character(class), length(class) == 1)
  pieces <- strsplit(generic, "::")[[1]]
  stopifnot(length(pieces) == 2)
  package <- pieces[[1]]
  generic <- pieces[[2]]
  caller <- parent.frame()
  get_method_env <- function() {
    top <- topenv(caller)
    if (isNamespace(top)) {
      asNamespace(environmentName(top))
    }
    else {
      caller
    }
  }
  get_method <- function(method) {
    if (is.null(method)) {
      get(paste0(generic, ".", class), envir = get_method_env())
    }
    else {
      method
    }
  }
  register <- function(...) {
    envir <- asNamespace(package)
    method_fn <- get_method(method)
    stopifnot(is.function(method_fn))
    if (exists(generic, envir)) {
      registerS3method(generic, class, method_fn, envir = envir)
    }
    # # commented because .rlang_s3_register_compat is not available, and
    # # this is just used to warn
    # else if (identical(Sys.getenv("NOT_CRAN"), "true")) {
    #   warn <- .rlang_s3_register_compat("warn")
    #   warn(c(sprintf("Can't find generic `%s` in package %s to register S3 method.",
    #                  generic, package), i = "This message is only shown to developers using devtools.",
    #          i = sprintf("Do you need to update %s to the latest version?",
    #                      package)))
    # }
  }
  setHook(packageEvent(package, "onLoad"), function(...) {
    register()
  })
  is_sealed <- function(pkg) {
    identical(pkg, "base") || environmentIsLocked(asNamespace(pkg))
  }
  if (isNamespaceLoaded(package) && is_sealed(package)) {
    register()
  }
  invisible()
}
