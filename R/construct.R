#' Build code to recreate an object
#'
#' `construct()` builds the code to reproduce one object, `construct_multi()`
#' builds the code to reproduce objects stored in a named list or environment.
#'
#' `construct_multi()` recognizes promises, this means that for instance
#'   `construct_multi(environment())` can be called in a
#'  function and will construct unevaluated arguments using `delayedAssign()`.
#'  Note however that `construct_multi(environment())` is equivalent to `construct_reprex()`
#'  called without argument and the latter is preferred.
#'
#' @param x An object, for `construct_multi()` a named list or an environment.
#'
#' @param data Named list or environment of objects we want to detect and mention by name (as opposed to
#'   deparsing them further). Can also contain unnamed nested lists, environments, or
#'   package names, in the latter case package exports and datasets will be considered.
#'   In case of conflict, the last provided name is considered.
#' @param pipe Which pipe to use, either `"base"` or `"magrittr"`.
#'   Defaults to `"base"` for R >= 4.2, otherwise to `"magrittr"`.
#' @param check Boolean. Whether to check if the created code reproduces the object
#'   using `waldo::compare()`.
#' @param compare Parameters passed to `waldo::compare()`, built with `compare_options()`.
#' @param ... Constructive options built with the `opts_*()` family of functions. See the "Constructive options"
#'   section below.
#' @param one_liner Boolean. Whether to collapse the output to a single line of code.
#' @param template A list of constructive options built with `opts_*()` functions,
#'   they will be overriden by `...`. Use it to set a default
#'   behavior for `{constructive}`.
#' @return An object of class 'constructive'.
#' @enumerateOptFunctions
#'
#' @export
#' @examples
#' construct(head(cars))
#' construct(head(cars), opts_data.frame("read.table"))
#' construct(head(cars), opts_data.frame("next"))
#' construct(iris$Species)
#' construct(iris$Species, opts_atomic(compress = FALSE), opts_factor("new_factor"))
construct <- function(x, ..., data = NULL, pipe = NULL, check = NULL,
                      compare = compare_options(), one_liner = FALSE,
                      template = getOption("constructive_opts_template")) {

  # reset globals
  globals$predefinition <- character()
  globals$envs <- data.frame(hash = character(), name = character())

  # check inputs
  .cstr_combine_errors(
    # force so we might fail outside of the try_fetch() when x is not properly provided
    force(x),
    check_dots_unnamed(),
    abort_wrong_data(data),
    abort_not_boolean(one_liner)
  )

  # process data into a flat named list of objects
  data <- process_data(data)

  # build code that produces the object, prepend with predefinitions if relevant
  caller <- caller_env()
  code <- try_construct(x, template = template, ..., data = data, pipe = pipe, one_liner = one_liner, env = caller)
  code <- c(globals$predefinition, code)

  # for https://github.com/cynkra/constructive/issues/101
  Encoding(code) <- "UTF-8"

  # attempt to parse, and style if successful
  styled_code <- try_parse(code, one_liner)

  # check output fidelity if relevant, signal issues and update globals$issues
  compare <- check_round_trip(x, styled_code, data, check, compare, caller)

  # build a new constructive object, leave the display work to the print method
  new_constructive(styled_code, compare)
}

#' @export
#' @rdname construct
construct_multi <- function(x, ..., data = NULL, pipe = NULL, check = NULL,
                            compare = compare_options(), one_liner = FALSE,
                            template = getOption("constructive_opts_template")) {
  abort_not_env_or_named_list(x)
  data <- process_data(data)

  if (is.list(x)) {
    constructives <- lapply(
      x, construct,  ...,
      data = data, pipe = pipe, check = check,
      compare = compare,
      one_liner = one_liner,
      template = template
    )
  } else if (is.environment(x)) {
    constructives <- list()
    for (nm in names(x)) {
      if (is_promise(as.symbol(nm), x)) {
        code <- do.call(substitute, list(as.name(nm), x))
        env <- promise_env(as.symbol(nm), x)
        code <- .cstr_apply(
          list(
            .cstr_construct(nm),
            value = deparse_call(code, style = FALSE),
            eval.env = .cstr_construct(env)
          ),
          "delayedAssign",
          recurse = FALSE
        )
        # FIXME: we don't collect issues yet here
        constructives[[nm]] <- new_constructive(code, NULL)
      } else {
        constructives[[nm]] <- construct(
          x[[nm]],
          ...,
          data = data, pipe = pipe, check = check,
          compare = compare,
          one_liner = one_liner,
          template = template
        )
      }
    }
  } else {
    abort("wrong input!")
  }

  code <- lapply(constructives, `[[`, "code")
  issues <- lapply(constructives, `[[`, "compare")
  issues <- Filter(Negate(is.null), issues)
  globals$issues <- issues
  code <-  Map(
    code, names(code),
    f = function(x, y) {
      if (startsWith(x[[1]], "delayedAssign(")) return(x)
      x[[1]] <- paste(protect(y), "<-", x[[1]])
      c(x, "")
    })
  code <- unlist(code)
  Encoding(code) <- "UTF-8"
  if (is.null(code)) code <- character(0)
  code <- as_constructive_code(unname(code))
  new_constructive(code, issues)
}

#' @export
print.constructive <- function(
    x,
    print_mode = getOption("constructive_print_mode", default = "console"),
    ...) {
  print_mode <- arg_match(
    print_mode,
    values = c("console", "script", "reprex", "clipboard"),
    multiple = TRUE
  )

  if ("console" %in% print_mode) {
    print(x$code)
  }
  if ("reprex" %in% print_mode) {
    check_installed("reprex")
    reprex_code <- c("reprex::reprex({", x$code, "})")
    eval.parent(parse(text = reprex_code))
  }
  if ("clipboard" %in% print_mode) {
    check_installed("clipr")
    clipr::write_clip(paste(x$code, collapse = "\n"), "character")
  }
  if ("script" %in% print_mode) {
    check_installed("rstudioapi")
    rstudioapi::documentNew(x$code, "r")
  }
  invisible(x)
}



# this is  styler:::print.vertical with small tweaks:
# * different default for `colored`
# * fail rather than warn if colored is TRUE and not installed
# * returns input invisibly

#' @export
print.constructive_code <- function(x, ..., colored = rlang::is_installed("prettycode"), style = NULL) {
  if (colored) {
    if (!is_installed("prettycode")) {
      abort(paste("Could not use `colored = TRUE`, as the package prettycode is not",
                  "installed. Please install it if you want to see colored output"))
    }
    x <- highlight_if_prettycode_installed(x, style = style)
  }
  cat(x, sep = "\n")
  invisible(x)
}

as_constructive_code <- function(x) {
  structure(x, class = "constructive_code")
}
