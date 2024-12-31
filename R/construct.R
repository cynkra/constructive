#' Build code to recreate an object
#'
#' * `construct()` builds the code to reproduce one object,
#' * `construct_multi()` builds the code to reproduce objects stored in a named
#'   list or environment.
#'
#' @details
#'
#' `construct_multi()` recognizes promises (also called lazy bindings),
#' this means that for instance `construct_multi(environment())` can be called
#' when debugging a function and will construct unevaluated arguments using
#' `delayedAssign()`.
#'
#' @seealso [construct_dput()] [construct_base()] [construct_clip()]
#'   [construct_dump()] [construct_reprex()] [construct_diff()]
#'
#' @param x An object, for `construct_multi()` a named list or an environment.
#' @param data Named list or environment of objects we want to detect and mention by name (as opposed to
#'   deparsing them further). Can also contain unnamed nested lists, environments, or
#'   package names, in the latter case package exports and datasets will be considered.
#'   In case of conflict, the last provided name is considered.
#' @param pipe Which pipe to use, either `"base"` or `"magrittr"`.
#'   Defaults to `"base"` for R >= 4.2, otherwise to `"magrittr"`.
#' @param check Boolean. Whether to check if the created code reproduces the object
#'   using `waldo::compare()`.
#' @param unicode_representation By default "ascii", which means only ASCII characters
#'   (code point < 128) will be used to construct strings and variable names. This makes sure that
#'   homoglyphs (different spaces and other identically displayed unicode characters)
#'   are printed differently, and avoid possible unfortunate copy and paste
#'   auto conversion issues. "latin" is more lax and uses all latin characters
#'   (code point < 256). "character" shows all characters, but not emojis. Finally
#'   "unicode" displays all characters and emojis, which is what `dput()` does.
#' @param escape Boolean. Whether to escape double quotes and backslashes. If `FALSE` we use
#'   single quotes to surround strings (including variable and element names)
#'   containing double quotes, and raw strings for strings that contain backslashes
#'   and/or a combination of single and double quotes. Depending on
#'   `unicode_representation` `escape = FALSE` cannot be applied on all strings.
#' @param pedantic_encoding Boolean. Whether to mark strings with the "unknown" encoding
#'   rather than an explicit native encoding ("UTF-8" or "latin1") when it's
#'   necessary to reproduce the binary representation exactly. This detail is
#'   normally of very little significance. The reason why we're not pedantic by default is that
#'   the constructed code might be different in the console and in snapshot
#'   tests and reprexes due to the latter rounding some angles, and it would
#'   be confusing for users.
#' @param compare Parameters passed to `waldo::compare()`, built with `compare_options()`.
#' @param ... Constructive options built with the `opts_*()` family of functions. See the "Constructive options"
#'   section below.
#' @param one_liner Boolean. Whether to collapse the output to a single line of code.
#' @param template A list of constructive options built with `opts_*()` functions,
#'   they will be overriden by `...`. Use it to set a default
#'   behavior for `{constructive}`.
#' @param classes A character vector of classes for which to use idiomatic
#'   constructors when available, we can provide a package instead of all its
#'   classes, in the "\{pkg\}" form, and we can use a minus sign (inside the quotes)
#'   to exclude rather than include. By default we use idiomatic constructors
#'   whenever possible. The special values `"*none*"` and `"*base*"` can be used
#'   to restrict the idiomatic construction to the objects. See `construct_dput()`
#'   and `construct_base()` for wrappers around this feature.
#' @param include_dotted Whether to include names starting with dots, this includes
#'   `.Random.seed` in the global environment and objects like `.Class` and
#'    `.Generic` in the execution environments of S3 methods.
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
#' construct_multi(list(a = head(cars), b = iris$Species))
construct <- function(
    x,
    ...,
    data = NULL,
    pipe = NULL,
    check = NULL,
    unicode_representation = c("ascii", "latin", "character", "unicode"),
    escape = FALSE,
    pedantic_encoding = FALSE,
    compare = compare_options(), one_liner = FALSE,
    template = getOption("constructive_opts_template"),
    classes = NULL) {

  # reset globals
  globals$predefinition <- character()
  globals$envs <- data.frame(hash = character(), name = character())
  globals$pedantic_encoding <- pedantic_encoding

  # check inputs
  .cstr_combine_errors(
    # force so we might fail outside of the try_fetch() when x is not properly provided
    force(x),
    check_dots_unnamed(),
    abort_wrong_data(data),
    abort_not_boolean(one_liner),
    abort_not_boolean(escape),
    { unicode_representation <- rlang::arg_match(unicode_representation) }
  )

  opts <- collect_opts(..., template = template)

  # process data into a flat named list of objects
  data <- process_data(data)
  classes <- process_classes(classes)

  # build code that produces the object, prepend with predefinitions if relevant
  caller <- user_env()
  code <- try_construct(
    x,
    opts = opts,
    template = template,
    data = data,
    pipe = pipe,
    unicode_representation = unicode_representation,
    unicode_representation.chr =
      opts$character$unicode_representation %||%
      opts$atomic$unicode_representation %||%
      unicode_representation,
    escape = escape,
    escape.chr =
      opts$character$escape %||%
      opts$atomic$escape %||%
      escape,
    one_liner = one_liner,
    env = caller,
    classes = classes
  )
  code <- c(globals$predefinition, code)

  # attempt to parse, and style if successful
  styled_code <- try_parse(code, one_liner)

  # check output fidelity if relevant, signal issues and update globals$issues
  compare <- check_round_trip(x, styled_code, data, check, compare, caller)

  # build a new constructive object, leave the display work to the print method
  new_constructive(styled_code, compare)
}

#' @export
#' @rdname construct
construct_multi <- function(
    x,
    ...,
    data = NULL,
    pipe = NULL,
    check = NULL,
    unicode_representation = c("ascii", "latin", "character", "unicode"),
    escape = FALSE,
    pedantic_encoding = FALSE,
    compare = compare_options(),
    one_liner = FALSE,
    template = getOption("constructive_opts_template"),
    classes = NULL,
    include_dotted = TRUE
    ) {
  abort_not_env_or_named_list(x)
  data <- process_data(data)
  unicode_representation <- match.arg(unicode_representation)

  if (is.list(x)) {
    if (!include_dotted) {
      nms <- grep("^[^.]|([.][.][.])", names(x), value = TRUE)
      x <- x[nms]
    }

    constructives <- lapply(
      x, construct,  ...,
      data = data, pipe = pipe, check = check,
      unicode_representation = unicode_representation,
      escape = escape,
      pedantic_encoding = pedantic_encoding,
      compare = compare,
      one_liner = one_liner,
      template = template,
      classes = classes
    )
  } else if (is.environment(x)) {
    opts <- collect_opts(..., template = template)
    constructives <- list()

    # ls() sorts names alphabetically while names() displays from most to least
    #   recently defined. Taking the reverse of names() will create the most
    #   natural looking reprexes
    nms <- rev(names(x))
    if (!include_dotted) {
      nms <- grep("^[^.]|([.][.][.])", nms, value = TRUE)
    }

    for (nm in nms) {
      if (rlang::env_binding_are_lazy(x, nm)) {
        code <- do.call(substitute, list(as.name(nm), x))
        env <- promise_env(as.symbol(nm), x)

        name_code <- .cstr_construct(
          nm,
          opts = opts,
          unicode_representation.chr = unicode_representation,
          escape.chr = escape,
          unicode_representation = unicode_representation,
          escape = escape
        )

        value_code <- deparse_call(
          code,
          style = FALSE,
          unicode_representation = unicode_representation,
          escape = escape,
          pedantic_encoding = pedantic_encoding
        )

        env_code <- .cstr_construct(
          env,
          opts = opts,
          unicode_representation.chr = unicode_representation,
          escape.chr = escape,
          unicode_representation = unicode_representation,
          escape = escape
        )

        code <- .cstr_apply(
          list(
            name_code,
            value = value_code,
            eval.env = env_code
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
          data = data,
          pipe = pipe,
          check = check,
          unicode_representation = unicode_representation,
          escape = escape,
          pedantic_encoding = pedantic_encoding,
          compare = compare,
          one_liner = one_liner,
          template = template,
          classes = classes
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
      y <- construct_string(y, unicode_representation, escape, mode = "name")
      x[[1]] <- paste(y, "<-", x[[1]])
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

  if ("clipboard" %in% print_mode) {
    check_installed("clipr")
    cli::cli_alert_info("Code has been added to the clipboard")
    clipr::write_clip(paste(x$code, collapse = "\n"), "character")
  }
  if ("console" %in% print_mode) {
    print(x$code)
  }
  if ("reprex" %in% print_mode) {
    check_installed("reprex")
    reprex_code <- c('getFromNamespace("prex", "reprex")({', x$code, "})")
    eval.parent(parse(text = reprex_code))
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
