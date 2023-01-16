#' Construct the session
#'
#' This builds code to reproduce the session. By default it prints comments describing
#'   the setup, attaching all non base packages present in the search path,
#'   and constructs all objects present in the global environment. Objects exported
#'   by attached package are used as data to sinplify  object construction.
#'
#' More often than not it is not a perfectly faithful reproduction of the session,
#' in particular it doesn't account (yet?) for:
#' * Namespaces loaded by other means than `library()` calls
#' * Modifications of namespaces
#' * Other environments on the search opath
#' * Options that have been set
#' * And most likely anything weirder than the above
#'
#' @param ... Forwarded to `construct_multi()`. Should not include a `data` argument,
#'   since `data` will be infered from attached packages.
#' @param session_info Whether to describe the setup in comments
#' @param library_calls Whether to include library calls
#'
#' @return An object of class 'constructive'.
#' @export
#' @examples
#' construct_session()
construct_session <- function(..., session_info = TRUE, library_calls = TRUE) {
  # session info ---------------------------------------------------------------
  if (session_info) {
  si <- sessionInfo()
  .rs.api.versionInfo <- NULL # to avoid note
  si_code <- paste("#", c(
    paste(si$R.version$version.string, if (Sys.getenv("RSTUDIO") == "1") paste("in RStudio", .rs.api.versionInfo()$version)),
    paste("running", si$running, "on", si$platform),
    paste("locale:", si$locale)
  ))
  }
  libs <- search()

  # library calls --------------------------------------------------------------
  if (library_calls){

  base_pkgs <- c("stats", "graphics", "grDevices", "utils", "datasets", "methods", "base")
  ignore_libs <- c(paste0("package:", base_pkgs), "devtools_shims", "tools:rstudio", "Autoloads")
  libs <- setdiff(libs, ignore_libs)
  libs <- grep("^package:", libs, value = TRUE)
  libs <- sub("package:", "", libs)
  data_libs <- libs
  # special case for tidyverse since it's quite common
  if ("tidyverse" %in% libs) {
    libs <- setdiff(libs, c("forcats", "stringr", "dplyr", "purrr", "readr", "tidyr", "tibble", "ggplot2"))
  }
  data_libs <- c(data_libs, base_pkgs)
  lib_code <- if (length(libs)) sprintf("library(%s)", rev(libs))
  } else {
    data_libs <- base_pkgs
  }

  # namespaces -----------------------------------------------------------------
  # TODO: attach namespaces that are not recursive deps of attached packages
  # TODO: detach relevant namespaces

  # options --------------------------------------------------------------------
  # TODO

  # objects --------------------------------------------------------------------
  objs <- setdiff(ls(.GlobalEnv, all.names = TRUE), ".Random.seed")
  objs <- mget(objs, .GlobalEnv, inherits = FALSE)
  # FIXME: is there a max_size that makes sense ?
  # total_size <- sum(sapply(objs, object.size))
  # if (total_size > max_size) {
  #   msg <- "The total size of objects must be less than `max_size`"
  #   info1 <- sprintf(
  #     "`max_size` is %s bytes, adjust the argument if necessary.",
  #     format(max_size, scientific = FALSE, big.mark = " ")
  #   )
  #   info2 <- sprintf(
  #     "The total size is %s bytes.",
  #     format(total_size, scientific = FALSE, big.mark = " ")
  #   )
  #   abort(c(msg, i = info1, x = info2))
  # }
  res <- construct_multi(objs, data = as.list(data_libs), ...)
  res$code <- c(si_code, lib_code, res$code)
  class(res$code) <- "vertical"
  res
}

get_set_options <- function() {
  # get default and actual options
  default_opts <- get_default_options()
  opts <- options()
  # ignore on both sides the options RStudio sets or changes
  # note: based on RStudio 4.2.1 on R 4.2.1
  rstudio_sets <- c(
    "askpass", "asksecret", "buildtools.check", "buildtools.with",
    "connectionObserver", "deparse.max.lines", "download.file.method",
    "ggvis.renderer", "help_type", "page_viewer", "plumber.docs.callback",
    "plumber.swagger.url", "profvis.keep_output", "profvis.print",
    "profvis.prof_extension", "profvis.prof_output", "restart", "reticulate.initialized",
    "reticulate.repl.busy", "reticulate.repl.hook", "reticulate.repl.initialize",
    "reticulate.repl.teardown", "rstudio.notebook.executing", "RStudioGD.antialias",
    "RStudioGD.backend", "shiny.launch.browser", "shinygadgets.showdialog",
    "terminal.manager", "viewer"
  )

  rstudio_changes <- c(
    "browser", "device", "echo", "HTTPUserAgent", "keep.source",
    "max.print", "menu.graphics", "pager", "pdfviewer", "width"
  )

  default_opts[c(rstudio_sets, rstudio_changes)] <- NULL
  opts[c(rstudio_sets, rstudio_changes)] <- NULL

  for (opt in names(opts)) {
    if (identical(opts[[opt]], default_opts[[opt]], ignore.environment = TRUE)) {
      opts[[opt]] <- NULL
      next
    }
    both_are_funs_with_same_body <-
      is.function(opts[[opt]]) &&
      is.function(default_opts[[opt]]) &&
      identical(as.list(opts[[opt]]), as.list(default_opts[[opt]]))
    if (both_are_funs_with_same_body) {
      opts[[opt]] <- NULL
    }
  }
  opts
}

# get_default_options <- function() {
#   default_opts <- callr::r(function(libs) {
#     lapply(libs, library, character.only = TRUE)
#     options()
#   }, list (libs = sub("^package:", "", grep("^package:", search(), value = TRUE))))
#   # remove options set by {callr}
#   callr_sets <- c("callr.rprofile_loaded", "error", "showErrorCalls")
#   default_opts[callr_sets] <- NULL
#   default_opts
# }
#
#
#
# compare_to_default <- function() {
#   opts <- options()
#   # ignore on both sides the options RStudio sets or changes
#   # note: based on RStudio 4.2.1 on R 4.2.1
#   rstudio_sets <- c(
#     "askpass", "asksecret", "buildtools.check", "buildtools.with",
#     "connectionObserver", "deparse.max.lines", "download.file.method",
#     "ggvis.renderer", "help_type", "page_viewer", "plumber.docs.callback",
#     "plumber.swagger.url", "profvis.keep_output", "profvis.print",
#     "profvis.prof_extension", "profvis.prof_output", "restart", "reticulate.initialized",
#     "reticulate.repl.busy", "reticulate.repl.hook", "reticulate.repl.initialize",
#     "reticulate.repl.teardown", "rstudio.notebook.executing", "RStudioGD.antialias",
#     "RStudioGD.backend", "shiny.launch.browser", "shinygadgets.showdialog",
#     "terminal.manager", "viewer"
#   )
#
#   rstudio_changes <- c(
#     "browser", "device", "echo", "HTTPUserAgent", "keep.source",
#     "max.print", "menu.graphics", "pager", "pdfviewer", "width"
#   )
#
#   rstudio_opts <- c(rstudio_sets, rstudio_changes)
#   opts[rstudio_opts] <- NULL
#   # we pass our system info to
#   diffs <- callr::r(function(si, opts, rstudio_opts) {
#     # libs in the correct attaching order
#     libs <- rev(c(names(si$otherPkgs), si$basePkgs))
#     lapply(libs, library, character.only = TRUE)
#
#     # namespaces
#
#     # callr loads several namespaces
#     ## Imports:
#     # processx (>= 3.6.1),  <- imports additionally ps
#     # R6,                   <- also imported by RStudio
#     # utils                 <- also imported by R
#     default_ns <- loadedNamespaces()
#     extra_ns <- setdiff(names(si$loadedOnly), default_ns)
#     lapply(extra_ns, loadNamespace)
#
#     # options
#
#     default_opts <- options()
#     callr_sets <- c("callr.rprofile_loaded", "error", "showErrorCalls")
#     default_opts[c(callr_sets, rstudio_opts)] <- NULL
#
#     for (opt in names(opts)) {
#       if (identical(opts[[opt]], default_opts[[opt]], ignore.environment = TRUE)) {
#         opts[[opt]] <- NULL
#         next
#       }
#       both_are_funs_with_same_body <-
#         is.function(opts[[opt]]) &&
#         is.function(default_opts[[opt]]) &&
#         identical(as.list(opts[[opt]]), as.list(default_opts[[opt]]))
#       if (both_are_funs_with_same_body) {
#         opts[[opt]] <- NULL
#       }
#     }
#
#     list(opts = opts, extra_ns = extra_ns)
#   }, list (si = sessionInfo(), opts = opts, rstudio_opts = rstudio_opts))
#   diffs
# }
