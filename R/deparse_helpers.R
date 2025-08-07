is_syntactic <- function(x) {
  x == make.names(x)
}

# exceptions -----------------------------------------------------------------

deparse_symbol <- function(call, check_syntactic, unicode_representation) {
  code <- construct_string(
    as.character(call),
    unicode_representation,
    escape = TRUE,
    mode = "symbol",
    protect = check_syntactic
  )
  code
}

deparse_syntactic_literal <- function(call, unicode_representation, escape) {
  .cstr_construct(
    call, template = NULL, data = NULL,
    unicode_representation = unicode_representation,
    escape = escape,
    unicode_representation.chr = unicode_representation,
    escape.chr = escape
  )
}

# function and control flow ---------------------------------------------------

deparse_function <- function(call, rec) {
  # no need to check more, already done by is_expression2
  pair_list_args <- sapply(call[[2]], rec)
  pair_list_code <- paste(protect(names(pair_list_args)), "=", pair_list_args)
  pair_list_code <- sub(" = $", "", pair_list_code)
  pair_list_code <- paste(pair_list_code, collapse = ", ")
  body_code <- rec(call[[3]])
  sprintf("function(%s) %s", pair_list_code, body_code)
}

deparse_if <- function(call, rec) {
  cond <- rec(call[[2]])
  yes <- rec(call[[3]])
  if (length(call) == 3) {
    return(sprintf("if (%s) %s", cond, yes))
  }
  no <- rec(call[[4]])
  sprintf("if (%s) %s else %s", cond, yes, no)
}

deparse_while <- function(call, rec) {
  cond <- rec(call[[2]])
  expr <- rec(call[[3]])
  sprintf("while (%s) %s", cond, expr)
}

deparse_for <- function(call, rec) {
  i <- rec(call[[2]])
  seq <- rec(call[[3]])
  expr <- rec(call[[4]])
  sprintf("for (%s in %s) %s", i, seq, expr)
}

deparse_repeat <- function(call, rec) {
  expr <- rec(call[[2]])
  sprintf("repeat %s", expr)
}

# surrounding ops ------------------------------------------------------------

deparse_subset <- function(call, rec, one_liner, indent, unicode_representation, escape) {
  arg1 <- rec(call[[2]])
  other_args <- deparse_named_args_to_string(
    call[-(1:2)],
    one_liner = one_liner,
    indent = indent,
    unicode_representation,
    escape
  )
  sprintf("%s[%s]", arg1, other_args)
}

deparse_subset2 <- function(call, rec, one_liner, indent, unicode_representation, escape) {
  arg1 <- rec(call[[2]])
  other_args <- deparse_named_args_to_string(
    call[-(1:2)],
    one_liner = one_liner,
    indent = indent,
    unicode_representation,
    escape
  )
  sprintf("%s[[%s]]", arg1, other_args)
}

is_regular_bracket_call <- function(call) {
  if (!identical(call[[1]], as.symbol("[")) && !identical(call[[1]], as.symbol("[["))) {
    return(FALSE)
  }
  if (length(call) < 3) {
    # even with empty bracket it is length 3 because x[] uses an empty arg
    return(FALSE)
  }

  if (identical(call[[2]], quote(expr=))) return(FALSE)
  if (!is.call(call[[2]])) return(TRUE)

  lhs_is_call_with_a_symbol_caller <-
    is.call(call[[2]]) &&
    is.symbol(call[[2]][[1]])

  if (!lhs_is_call_with_a_symbol_caller) return(TRUE)
  lhs_caller_chr <- as.character((call[[2]][[1]]))
  if (is_cf(lhs_caller_chr) || lhs_caller_chr == "function") return(FALSE)
  precedence(lhs_caller_chr, length(call[[2]])) >= 16
}

deparse_paren <- function(call, rec) {
  sprintf("(%s)", rec(call[[2]]))
}

deparse_curly <- function(call, rec, one_liner, indent) {
  if (length(call) == 1) {
    return("{ }")
  }
  # tunneling
  if (rlang::is_call(call[[2]], "{") && is.symbol(call[[c(2, 2)]])) {
    return(sprintf("{{ %s }}", as.character(call[[c(2, 2)]])))
  }

  if (one_liner) {
    args <- paste(vapply(call[-1], rec, character(1)), collapse = "; ")
    return(sprintf("{%s}", args))
  }
  args <- vapply(call[-1], rec, character(1), indent = indent + 1)
  args <- paste(indent(args, depth = indent + 1), collapse = "\n")
  sprintf("{\n%s\n%s}", args, indent("", depth = indent))
}

# infix ops ------------------------------------------------------------------

is_unary <- function(x) {
  x %in% c("-", "+", "!", "?", "^", "~", "?")
}

is_infix_wide <- function(x) {
  x %in% c("+", "-", "*", "/", "<", ">", "<=", ">=", "==", "!=", "&", "&&", "|", "||", "~", "<-", "<<-", "=", "?", ":=") || grepl("^%.*%$", x)
}

is_infix_narrow <- function(x) {
  x %in% c("::", ":::", "$", "@", "^", ":")
}

is_op <- function(x) {
  is_unary(x) || is_infix_wide(x) || is_infix_narrow(x)
}

is_cf <- function(x) {
  x %in% c("if", "while", "for", "repeat")
}

deparse_unary <- function(caller, call, rec) {
  if (caller %in% c("+", "-")) {
    # FIXME: pipe = FALSE is too restrictive
    # should apply only to direct arg but not recursively
    sprintf("%s%s", caller, rec(call[[2]], pipe = FALSE))
  } else {
    sprintf("%s%s", caller, rec(call[[2]]))
  }

}

deparse_infix_wide <- function(caller, call, rec, pipe) {
  # cancel the pipe where it doesn't belong
  pipe <- pipe && caller %in% c("~", "<-", "<<-", "=", "?", ":=")
  # FIXME: we probably want to get rid of this
  use_right_assignment <-
    caller == "<-" &&
    is.call(call[[2]]) &&
    list(call[[2]][[1]]) %in% alist(`<-`, `if`, `for`, `while`, `repeat`)

  if (use_right_assignment) {
    # because `<-` has differen precedence
    if (identical(call[[2]][[1]], as.symbol("<-"))) {
      code <- sprintf(
        "%s -> %s <- %s",
        rec(call[[2]][[3]]),
        rec(call[[2]][[2]]),
        rec(call[[3]])
      )
      return(code)
    }

    code <- sprintf(
      "%s -> %s",
      rec(call[[3]]),
      rec(call[[2]])
    )
    return(code)
  }

  sprintf(
    "%s %s %s",
    rec(call[[2]]),
    caller,
    rec(call[[3]])
  )
}

deparse_double_triple_colon <- function(caller, call, rec) {
  sprintf("%s%s%s", rec(call[[2]]), caller, rec(call[[3]]))
}

deparse_accessor <- function(caller, call, rec, unicode_representation, escape) {
  if (is.symbol(call[[3]])) {
    nm <- as.character(call[[3]])
    nm <- construct_string(nm, unicode_representation, escape, mode = "symbol")
    return(sprintf("%s%s%s", rec(call[[2]]), caller, nm))
  }
  if (is.character(call[[3]])) {
    nm <- construct_string(
      call[[3]],
      unicode_representation = unicode_representation,
      escape = escape
    )
    return(sprintf('%s%s%s', rec(call[[2]]), caller, nm))
  }
}

deparse_hat_colon <- function(caller, call, rec) {
  # FIXME: pipe = FALSE is too restrictive
  # should apply only to direct arg but not recursively
  sprintf("%s%s%s", rec(call[[2]]), caller, rec(call[[3]], pipe = FALSE))
}

# lisp -------------------------------------------------------------------------

deparse_pipe <- function(caller, call, rec, one_liner, indent, unicode_representation, escape, protect) {
  if (protect) caller <- protect(caller)
  args <- deparse_named_args_to_string(
    call[-1],
    one_liner = one_liner,
    indent = indent,
    unicode_representation,
    escape
  )
  sprintf("%s(%s)", caller, args)
}

deparse_lisp <- function(caller, call, rec, one_liner, indent, unicode_representation, escape, protect) {
  if (protect) caller <- protect(caller)
  args <- deparse_named_args_to_string(
    call[-1],
    one_liner = one_liner,
    indent = indent,
    unicode_representation,
    escape
  )
  sprintf("%s(%s)", caller, args)
}

# other helpers ----------------------------------------------------------------

deparse_named_args_to_string <- function(args, one_liner, indent, unicode_representation, escape) {
  if (length(args) == 0) {
    return("")
  }
  args <- vapply(args, deparse_call_impl, character(1), one_liner = one_liner, indent = indent + 1, lisp_equal = TRUE)
  nms0 <- rlang::names2(args)
  nms <- construct_strings(nms0, unicode_representation, escape, mode = "name")
  args <- ifelse(nms0 == "", args, paste(nms, "=", args))
  # FIXME: the 80 is a bit arbitrary, since we don't account for indent and length of caller
  if (one_liner || max(nchar(args)) < 80) return(paste(args, collapse = ", "))
  args <- paste(indent(args, depth = indent + 1), collapse = ",\n")
  paste0("\n", args, "\n", indent("", depth = indent))
}

precedence <- function(x, call_length = 2) {
  # length(x) > 1 means x was produced from a call, like `pkg::foo`, so it
  # has the highest precedence
  if (length(x) > 1) return(Inf)
  if (!call_length %in% c(2, 3)) return(Inf)
  if (call_length == 2) {
    precedences <- c(
      "-" = 14,
      "+" = 14,
      "!" = 8,
      "~" = 5,
      "?" = 1
    )
  } else {
    if (grepl("^%.*%$", x)) return(12)
    precedences <- c(
      "::" = 18,
      ":::" = 18,
      "$" = 17,
      "@" = 17,
      "[" = 16,
      "[[" = 16,
      "^" = 15,
      # "-" = 14,
      # "+" = 14,
      ":" = 13,
      #"%any%", # 12
      "|>" = 12,
      "*" = 11,
      "/" = 11,
      "+" = 10,
      "-" = 10,
      "<" = 9,
      ">" = 9,
      "<=" = 9,
      ">=" = 9,
      "==" = 9,
      "!=" = 9,
      #"!" = 8,
      "&" = 7,
      "&&" = 7,
      "|" = 6,
      "||" = 6,
      "~" = 5,
      "->" = 4,
      "->>" = 4,
      "<-" = 3,
      "<<-" = 3,
      "=" = 2,
      "?" = 1
    )
  }
  # if the caller is not found above, it is a regular function call foo(x)
  # so it has the highest precedence
  if (!x %in% names(precedences)) return(Inf)
  precedences[[x]]
}

# checks if the operator has a higher precedence than both the lhs and rhs
# of the call
operands_have_higher_or_equal_precedence <- function(operator, call) {
  if (any(sapply(call[-1], identical, quote(expr=)))) return(FALSE)
  if (!length(call) %in% c(2, 3)) return(TRUE)

  # we need to special case ops with righ to left precedence
  lhs <- call[[2]] # actually rhs when call is length 2
  op_prec <-  precedence(operator, length(call))
  if (is.call(lhs)) {
    lhs_caller_chr <- as.character(lhs[[1]])
    if (length(lhs_caller_chr) == 1 && lhs_caller_chr %in% c("[", "[[")) {
      lhs_prec <- Inf
    } else if (length(call) == 3 && length(lhs_caller_chr) == 1 && is_cf(lhs_caller_chr)) {
      lhs_prec <- 1.5 # just above `?`
      } else {
      lhs_prec <- precedence(lhs_caller_chr, length(lhs))
    }
  } else {
    lhs_prec <- Inf
  }
  if (length(call) == 3 && is.call(rhs <- call[[3]])) {
    rhs_prec <- precedence(as.character(rhs[[1]]), length(rhs))
  } else {
    rhs_prec <- Inf
  }

  # `=`, `<-`, and `^` have right to left precedence
  if (op_prec %in% c(2, 3, 15)) {
    return(lhs_prec > op_prec && rhs_prec >= op_prec)
  }
  lhs_prec >= op_prec && rhs_prec > op_prec
}
