#' Deparse a language object
#'
#' An alternative to `base::deparse()` and `rlang::expr_deparse()` that
#' handles additional corner cases and fails when encountering tokens other than
#' symbols and syntactic literals where cited alternatives would produce non syntactic code.\cr\cr
#'
#' @param call A call.
#' @param one_liner Boolean. Whether to collapse multi-line expressions on a single line using
#'   semicolons.
#' @param pipe Boolean. Whether to use the base pipe to disentangle nested calls. This
#'   works best on simple calls.
#' @param style Boolean. Whether to give a class "constructive_code" on the output
#'   for pretty printing.
#' @param collapse Boolean. Whether to collapse the output to a single string,
#'   won't be directly visible if `style` is `TRUE`.
#' @inheritParams construct
#'
#' @return a string or a character vector, with a class "constructive_code" for pretty
#'   printing if `style` is `TRUE`.
#' @export
#'
#' @examples
#' expr <- quote(foo(bar({this; that}, 1)))
#' deparse_call(expr)
#' deparse_call(expr, one_liner = TRUE)
#' deparse_call(expr, pipe = TRUE)
#' deparse_call(expr, style = FALSE)
deparse_call <- function(
    call,
    one_liner = FALSE,
    pipe = FALSE,
    style = TRUE,
    collapse = !style,
    unicode_representation = c("ascii", "latin", "character", "unicode"),
    escape = FALSE,
    pedantic_encoding = FALSE) {

  .cstr_combine_errors(
    abort_not_boolean(one_liner),
    abort_not_boolean(pipe),
    abort_not_boolean(style),
    abort_not_boolean(collapse),
    { unicode_representation <- rlang::arg_match(unicode_representation) },
    abort_not_boolean(escape)
  )

  globals$pedantic_encoding <- pedantic_encoding

  code <- rlang::try_fetch(
    deparse_call_impl(
      call,
      one_liner,
      0,
      pipe,
      check_syntactic = TRUE,
      unicode_representation,
      escape,
      lisp_equal = TRUE
    ),
    error = function(cnd) {
      abort("`call` must only be made of symbols and syntactic literals", parent = cnd)
    })
  if (!collapse) {
    code <- split_by_line(code)
  }
  if (style) {
    code <- as_constructive_code(code)
  }
  code
}

# a dot absorbing stripped down version of deparse_call() to be used internally
deparse_call0 <- function(
    call,
    one_liner = FALSE,
    unicode_representation = c("ascii", "latin", "character", "unicode"),
    escape = FALSE,
    ...) {
  code <- deparse_call_impl(
    call,
    one_liner = one_liner,
    unicode_representation = unicode_representation,
    escape = escape,
    lisp_equal = TRUE
  )
  split_by_line(code)
}

deparse_call_impl <- function(
    call,
    one_liner = FALSE,
    indent = 0,
    pipe = FALSE,
    check_syntactic = TRUE,
    unicode_representation = "ascii",
    escape = FALSE,
    lisp_equal = FALSE, # To handle `=` as top level caller, e.g. quote(`=`(x, 1))
    force_lisp = FALSE # To prevent callers from using the infix form, e.g. `+`(x, y)(z)
) {

  # helper to avoid forwarding all args all the time
  rec <- function(call, ...) {
    # override defaults
    if (...length()) list2env(list(...), environment())
    deparse_call_impl(
      call,
      one_liner,
      indent,
      pipe,
      check_syntactic,
      unicode_representation,
      escape,
      force_lisp = force_lisp
    )
  }

  if (is.symbol(call))
    return(deparse_symbol(call, check_syntactic, unicode_representation, escape))

  check_syntactic <- TRUE

  # artificial cases where caller is NULL, a numeric etc
  if (rlang::is_syntactic_literal(call))
    return(deparse_syntactic_literal(call, unicode_representation, escape))

  if (!is.call(call)) {
    code <- paste(capture.output(construct(call, check = FALSE)), collapse = "\n")
    msg <- sprintf("found element of type '%s' and length '%s':\n%s", typeof(call), length(call), code)
    abort(msg)
  }
  caller_lng <- call[[1]]
  # if the caller is not a symbol in order to parse we need to express it in lisp form
  # for instance `+`(1, 2)(3), hence force_lisp() below.
  # This does NOT apply if the caller is a call to `::` or `:::`!
  caller_calls_colon_ops <-
    is.call(caller_lng) && list(caller_lng[[1]]) %in% list(
      quote(`::`), quote(`:::`), quote(`$`), quote(`@`)
    )
  caller <- rec(
    caller_lng,
    check_syntactic = FALSE,
    force_lisp = !caller_calls_colon_ops
  )
  if (is_op(caller) && force_lisp) {
    return(deparse_lisp(
      caller, call, rec, one_liner, indent, unicode_representation, escape,
      protect = TRUE
    ))
  }
  force_lisp <- FALSE

  if (lisp_equal && caller == "=") {
    return(deparse_lisp(
      caller, call, rec, one_liner, indent, unicode_representation, escape,
      protect = TRUE
    ))
  }

  # function and control flow ---------------------------------------------------

  if (caller == "function")
    return(deparse_function(call, rec))

  if (caller == "if" && length(call) %in% 3:4)
    return(deparse_if(call, rec))

  if (caller == "while" && length(call) == 3)
    return(deparse_while(call, rec))

  if (caller == "for" && length(call) == 4)
    return(deparse_for(call, rec))

  if (caller == "repeat" && length(call) == 2)
    return(deparse_repeat(call, rec))

  # surrounding ops ------------------------------------------------------------

  if (caller == "[" && length(call) > 1)
    return(deparse_subset(call, rec, one_liner, indent, unicode_representation, escape))

  if (caller == "[[" && length(call) > 1)
    return(deparse_subset2(call, rec, one_liner, indent, unicode_representation, escape))

  if (caller == "(" && length(call) == 2)
    return(deparse_paren(call, rec))

  if (caller == "{")
    return(deparse_curly(call, rec, one_liner, indent))

  # non standard use of infix ops ----------------------------------------------

  if (is_op(caller) && !operands_have_higher_or_equal_precedence(caller, call)) {
    args <- deparse_named_args_to_string(
      call[-1],
      one_liner = one_liner,
      indent = indent,
      unicode_representation,
      escape
    )
    return(sprintf("%s(%s)", protect(caller), args))
  }

  # infix ops ------------------------------------------------------------------

  if (is_unary(caller) && length(call) == 2)
    return(deparse_unary(caller, call, rec))

  if (is_infix_wide(caller) && length(call) == 3)
    return(deparse_infix_wide(caller, call, rec, pipe))

  if (
    caller %in% c("::", ":::") &&
    length(call) == 3 &&
    (is.symbol(call[[2]]) || is.character(call[[2]])) &&
    (is.symbol(call[[3]]) || is.character(call[[3]]))
  ) {
    return(deparse_double_triple_colon(caller, call, rec))
  }

  if (caller %in% c("@", "$") && length(call) == 3 &&
      (is.symbol(call[[3]]) || is.character(call[[3]])))
    return(deparse_accessor(caller, call, rec, unicode_representation, escape))

  if (caller %in% c("^", ":") && length(call) == 3)
    return(deparse_hat_colon(caller, call, rec))

  # lisp calls  ----------------------------------------------------------------

  if (pipe && length(call) > 1 && rlang::names2(call)[[2]] == "") {
    arg1 <- rec(call[[2]])
    arg1_is_pipeable <-
      !is.call(call[[2]]) ||
      operands_have_higher_or_equal_precedence("|>", list(NULL, call[[2]], NULL))
    if (arg1_is_pipeable) {
      if (is.symbol(caller_lng)) {
        caller <- protect(caller)
      }
      other_args <- vapply(call[-(1:2)], rec, character(1))
      other_args <- paste(rlang::names2(other_args), "=", other_args)
      other_args <- sub("^ = ", "", other_args)
      return(sprintf(
        "%s %s %s(%s)",
        arg1,
        get_pipe_symbol(NULL),
        caller,
        paste(other_args, collapse = ", ")
      ))
    }
  }

  deparse_lisp(
    caller, call, rec, one_liner, indent, unicode_representation, escape,
    protect = is.symbol(caller_lng)
  )
}
