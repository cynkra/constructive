#' Deparse a language object
#'
#' This is an alternative to `base::deparse()` and `rlang::expr_deparse()` that
#' handles additional corner cases and fails when encountering tokens other than
#' symbols and syntactic literals where cited alternatives would produce non syntactic code.\cr\cr
#' Moreover it will produce a one liner if `one_liner = TRUE` and
#' will attempt to use the base pipe on nested calls if `pipe = TRUE`.\cr\cr
#' By default the output is styled with {styler}, if style is set to `FALSE` a regular
#' string is returned, using `"\n"` for new lines.
#'
#' @param call A call
#' @param one_liner Boolean. Whether to collapse multi-line expressions on a single line using
#'   semicolons
#' @param pipe Boolean. Whether to use the base pipe to disentangle nested calls. This
#'   works best on simple calls.
#' @param style Boolean. Whether to use `styler::style_text()` on thr output
#'
#' @return a string or a styled character vector
#' @export
#'
#' @examples
#' expr <- quote(foo(bar({this; that}, 1)))
#' deparse_call(expr)
#' deparse_call(expr, one_liner = TRUE)
#' deparse_call(expr, pipe = TRUE)
#' deparse_call(expr, style = FALSE)
#' # some corner cases are handled better than in base R
#' deparse(call("$", 1, 1)) # returns non syntactic output
#' deparse_call(call("$", 1, 1))
deparse_call <- function(call, one_liner = FALSE, pipe = FALSE, style = TRUE) {
  code <- rlang::try_fetch(
    deparse_call_impl(call, one_liner, 0, pipe),
    error = function(cnd) {
      abort("`call` must only be made of symbols and syntactic literals", parent = cnd)
    })
  if (style) {
    scope <- if (one_liner) "indention" else "line_breaks"
    code <- styler::style_text(code, scope = scope)
  }
  code
}

deparse_call_impl <- function(call, one_liner = FALSE, indent = 0, pipe = FALSE, check_syntactic = TRUE) {
  if(is.symbol(call)) {
    code <- as.character(call)
    if (check_syntactic && code != "" && !is_syntactic(code)) {
      code <- sprintf("`%s`", code)
    }
    return(code)
  }
  # artificial cases where caller is NULL, a numeric etc
  if (rlang::is_syntactic_literal(call)) {
    return(construct_idiomatic(call, template = NULL))
  }
  if (!is.call(call)) {
    code <- paste(capture.output(construct(call, check = FALSE)), collapse = "\n")
    msg <- sprintf("found element of type '%s' and length '%s':\n%s", typeof(call), length(call), code)
    abort(msg)
  }
  caller_lng <- call[[1]]
  caller <- deparse_call_impl(caller_lng, check_syntactic = FALSE)

  if (caller == "function") {
    # no need to check more, already done by is_expression2
    pair_list_args <- sapply(call[[2]], deparse_call_impl)
    pair_list_code <- paste(names(pair_list_args), "=", pair_list_args)
    pair_list_code <- sub(" = $", "", pair_list_code)
    pair_list_code <- paste(pair_list_code, collapse = ", ")
    body_code <- deparse_call_impl(call[[3]], one_liner, indent, pipe)
    code <- sprintf("function(%s) %s", pair_list_code, body_code)
    return(code)
  }

  if (caller == "if" & length(call) %in% 3:4) {
    cond <- deparse_call_impl(call[[2]], one_liner, indent)
    yes <- deparse_call_impl(call[[3]], one_liner, indent)
    if (length(call) == 3) {
      return(sprintf("if (%s) %s", cond, yes))
    } else if (length(call) == 4) {
      no <- deparse_call_impl(call[[4]], one_liner, indent)
      return(sprintf("if (%s) %s else %s", cond, yes, no))
    }
  }

  if (caller == "while" & length(call) == 3) {
    cond <- deparse_call_impl(call[[2]], one_liner, indent)
    expr <- deparse_call_impl(call[[3]], one_liner, indent)
    return(sprintf("while (%s) %s", cond, expr))
  }

  if (caller == "for" & length(call) == 4) {
    i <- deparse_call_impl(call[[2]], one_liner, indent)
    seq <- deparse_call_impl(call[[3]], one_liner, indent)
    expr <- deparse_call_impl(call[[4]], one_liner, indent)
    return(sprintf("for (%s in %s) %s", i, seq, expr))
  }

  if (caller == "repeat" & length(call) == 2) {
    expr <- deparse_call_impl(call[[2]], one_liner, indent)
    return(sprintf("repeat %s", expr))
  }

  if(is_unary(caller) && length(call) == 2) {
    return(sprintf("%s%s", caller, deparse_call_impl(call[[2]], one_liner, indent)))
  }

  if(is_infix_wide(caller) && length(call) == 3) {
    # cancel the pipe where it doesn't belong
    pipe <- pipe && caller %in% c("~", "<-", "<<-", "=" ,"?", ":=")
    code <- sprintf(
      "%s %s %s" ,
      deparse_call_impl(call[[2]], one_liner, indent, pipe),
      caller,
      deparse_call_impl(call[[3]], one_liner, indent, pipe)
    )
    return(code)
  }


  if (caller %in% c("::", ":::") && length(call) == 3 && is.character(call[[2]]) && is.character(call[[3]])) {
    return(sprintf("%s%s%s" , call[[2]], caller, call[[3]]))
  }

  if (caller %in% c("@", "$") && length(call) == 3 && is.character(call[[3]])) {
    return(sprintf("%s%s%s" , deparse_call_impl(call[[2]], one_liner, indent), caller, call[[3]]))
  }

  if (caller %in% c("^", ":") && length(call) == 3) {
    return(sprintf("%s%s%s" , deparse_call_impl(call[[2]], one_liner, indent), caller, deparse_call_impl(call[[3]], one_liner, indent)))
  }

  if (caller == "[" && length(call) > 1) {
    arg1 <- deparse_call_impl(call[[2]])
    other_args <- paste(vapply(call[-(1:2)], deparse_call_impl, character(1), one_liner = one_liner, indent = indent), collapse = ", ")
    return(sprintf("%s[%s]", arg1, other_args))
  }

  if (caller == "[[" && length(call) > 1) {
    arg1 <- deparse_call_impl(call[[2]])
    other_args <- paste(vapply(call[-(1:2)], deparse_call_impl, character(1), one_liner = one_liner, indent = indent), collapse = ", ")
    return(sprintf("%s[[%s]]", arg1, other_args))
  }

  if (caller == "(" && length(call) == 2) {
    return(sprintf("(%s)", deparse_call_impl(call[[2]], one_liner, indent, pipe)))
  }

  if (caller == "{" && length(call) > 1) {
    # tunneling
    if (rlang::is_call(call[[2]], "{") && is.symbol(call[[c(2,2)]])) {
      return(sprintf("{{ %s }}", as.character(call[[c(2, 2)]])))
    }

    if (one_liner) {
      args <- paste(vapply(call[-1], deparse_call_impl, character(1), one_liner = one_liner, indent = indent, pipe = pipe), collapse = "; ")
      return(sprintf("{%s}", args))
    }
    sep <- paste0("\n", strrep(" ", indent))
    args <- vapply(call[-1], deparse_call_impl, character(1), one_liner = one_liner, indent = indent + 2, pipe = pipe)
    args <- paste0( strrep(" ", indent + 2), args)
    args <- paste(args, collapse = "\n")
    return(sprintf("{\n%s\n%s}", args, strrep(" ", indent)))
  }

  if (is.symbol(caller_lng) && !is_syntactic(caller)) {
    caller <- sprintf("`%s`", caller)
  }

  if (pipe && length(call) > 1 && rlang::names2(call)[[2]] == "") {
    arg1 <- deparse_call_impl(call[[2]], one_liner, indent, pipe)
    other_args <- vapply(call[-(1:2)], deparse_call_impl, character(1), one_liner = one_liner, indent = indent)
    other_args <- paste(rlang::names2(other_args), "=", other_args)
    other_args <- sub("^ = ", "", other_args)
    if (!is.call(call[[2]]) || endsWith(arg1, ")") || endsWith(arg1, "}")) {
    return(sprintf("%s |> %s(%s)", arg1, caller, paste(other_args, collapse = ", ")))
    }
  }
  args <- vapply(call[-1], deparse_call_impl, character(1), one_liner = one_liner, indent = indent)
  args <- paste(rlang::names2(args), "=", args)
  args <- sub("^ = ", "", args)
  sprintf("%s(%s)", caller, paste(args, collapse = ", "))
}

is_syntactic <- function(x) {
  x == make.names(x)
}

is_unary <- function(x) {
  x %in% c("-", "+", "!", "?", "^", "~", "?")
}

is_infix_wide <- function(x) {
  x %in% c("+", "-", "*", "/", "<", ">", "<=", ">=", "==", "!=", "&", "&&", "|", "||", "~", "<-", "<<-", "=" ,"?", ":=") || grepl("^%.*%$", x)
}

is_infix_narrow <- function(x) {
  x %in% c("::", ":::", "$", "@", "^", ":")
}


