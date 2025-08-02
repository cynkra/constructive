#' @export
#' @rdname other-opts
opts_Guide <- function(constructor = c("default", "next", "environment"), ...) {
  .cstr_options("Guide", constructor = constructor[[1]], ...)
}

#' @export
#' @method .cstr_construct Guide
.cstr_construct.Guide <- function(x, ...) {
  opts <- list(...)$opts$Guide %||% opts_Guide()
  if (is_corrupted_Guide(x) || opts$constructor == "next") return(NextMethod())
  UseMethod(".cstr_construct.Guide", structure(NA, class = opts$constructor))
}

is_corrupted_Guide <- function(x) {
  # TODO
  FALSE
}

#' @export
#' @method .cstr_construct.Guide environment
.cstr_construct.Guide.environment <- function(x, ...) {
  .cstr_construct.environment(x, ...)
}

#' @export
#' @method .cstr_construct.Guide default

.cstr_construct.Guide.default <- function(x, ...) {
  # first check if it's one of the default guides in the package
  ggplot2 <- asNamespace("ggplot2")
  guides <- ls(ggplot2, pattern = "Guide*")
  ind <- vapply(mget(guides, envir = ggplot2), identical, logical(1), x)
  if (any(ind)) return(paste0("ggplot2::", guides[ind]))

  # new_guide() works only with a `super`, if we don't have one we need to
  # use ggproto() directly.
  if (is.null(x$super)) return(.cstr_construct.ggproto(x, ...))
  args <- environment(x$super)$env$args # x$params
  args$available_aes <- x$available_aes
  # there is a weird coercion going on at least for the `order` variable
  # so we can't take it from `args`, we attempt a workaround below
  if (!is.null(args$order) && !is.null(environment(x$super)$env$args$order)) {
    args$order <- environment(x$super)$env$args$order
  }
  # it seems like when `direction` is absent from x$params but present in
  # `environment(gb$super)$env$args` it means it was set manually to NULL
  # if ("direction" %in% names(environment(x$super)$env$args)) {
  #   args["direction"] <- list(direction = environment(x$super)$env$args)
  # }


  args <- keep_only_non_defaults(args, ggplot2::new_guide)
  if (identical(args$hash, character(0))) args$hash <- NULL
  # if (is.null(args$direction)) args$direction <- NULL
  args$super <- eval(eval(quote(`_inherit`), environment(x$super)), environment(x$super)$env, NULL)
  code <- .cstr_apply(args, "ggplot2::new_guide", ...)
  if (!identical(environment(x$super)$env$pf, list(...)$env)) {
    env_code <- .cstr_construct(environment(x$super)$env$pf, ...)
    if (list(...)$one_liner) {
      code <- .cstr_apply("eval_q", list(code, env_code), recurse = FALSE, ...)
    } else {
      eval_q_code <- .cstr_wrap(env_code, "evalq", new_line = FALSE)
      code <- .cstr_pipe(code, eval_q_code, ...)
    }
  }
  code
}

repair_attributes_Guide <- function(x, code, ...) {
  code
}

