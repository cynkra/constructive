# what to do with dots ?
# they need to be evaled in different environments and might contain NSE too
# we night try to eval what we can in the correct env
# if env is not parent env we should store those in lists, maybe named ...1 etc
# so we'll have foo(x = ...1$x + ...$y) or foo(x = with(...1, x + y))


# FIXME: calling on a call or without arg in the call should be exactly the same and use more of the same code
# FIXME: have a safe mode, arg name TBD so we construct only quosures, or list of quosures for dots,
#  and then call inject, so we don't force any evaluation.
# * If the expr is not a call or a symbol we can construct it as is without the quo
# * If env is .GlobalEnv the object can also be defined as is and the expression directly fed to the call
# * If env of all dots is .GlobalEnv we can define the object as
construct_reprex <- function(x, quote = TRUE, ..., check = NULL, template = NULL) {
  env <- parent.frame()
  nse_msgs <- character()

  if (missing(x)) {
    call <- sys.call(-1)
    call_matched <- match.call(sys.function(-1), call, envir = parent.frame(2))
    call_expanded <- eval.parent(bquote(substitute(.(call))))
    call_for_nms <- call_matched
    dot_pos <- which(sapply(call_matched[-1], function(x) is.symbol(x) && grepl("^\\.\\.\\d+$", as.character(x))))
    for(dot in dot_pos) {
      val <- try(eval(call_matched[[dot + 1]], env), silent = TRUE)
      if (inherits(val, "try-error")) {
        call_matched[[dot + 1]] <- call_expanded[[dot + 1]]
        call_for_nms[[dot + 1]] <- NULL
      }
    }
    nms <- all.names(call_for_nms[-1], unique = TRUE)
    call <- call_matched
  } else {
    call <- if (quote) substitute(x) else x
    nms <- all.names(call, unique = TRUE)
  }
  objs <- sapply(nms, function(x) try(eval(as.symbol(x), env), silent = TRUE), simplify = FALSE)
  errors_lgl <- sapply(objs, inherits, "try-error")
  objs <- objs[!errors_lgl]
  data <- preprocess_data(list("base", "methods", "datasets", "utils", "grDevices", "stats")) # FIXME: graphics doesn't work because plot's env is base
  code <- lapply(objs, construct_raw, template = template, data = data, ...)
  useful_code <- code[!mapply(code, names(code), FUN = identical)]
  useful_code_with_assignments <- Map(
    useful_code, names(useful_code),
    f = function(x, y) {
      x[[1]] <- paste(y, "<-", x[[1]])
      c(x, "")
    })
  out <- c(unlist(useful_code_with_assignments), deparse_call(call))
  if (any(errors_lgl)) {
    msg <- "Some variable bindings couldn't be found so objects were not reproduced."
    info <- sprintf("Non existent binding for %s", toString(shQuote(nms[errors_lgl])))
    rlang::inform(c(msg, i = info))
  }
  out <- styler::style_text(out)
  if (missing(x)) do.call("return", list(out), envir = sys.frame(1))
  out
}

construct_reprex <- function(x, quote = TRUE, ..., check = NULL, template = NULL) {
  ## was a call provided ?
  if (missing(x)) {
    ## fetch call and fun from stack, fetch env 2 frames above
     call <- sys.call(-1)
     env <- parent.frame(2)
     fun <- sys.function(-2)
  } else {
    ## capture call, fetch env 1 frame above, eval fun
    call <- substitute(x)
    env <- parent.frame()
    #fun <- sys.function(-1)
    eval(call[[1]], env)
  }
  ## create matched call, will contain all named arg and ..n for dots, deduce args
  call_matched <- match.call(fun, call, expand = FALSE, envir = env)
  args <- as.list(call_matched[-1])
  # all args are named since the call is matched and expand is FALSE
  objs <- list()
  #browser()
  for (arg_nm in names(args)) {
    if (arg_nm == "...") {
      objs[["..."]] <- evalq(rlang::enquos(...), env)
      args[["..."]] <- quote(!!!..args..$...)
      names(args)[names(args) == "..."] <- ""
      next
    }
    objs[[arg_nm]] <-  eval(substitute(enquo(ARG), list(ARG = .(sym(arg_nm)))), env)
    args[[arg_nm]] <- substitute(!!..args..$ARG, list(ARG = .(sym(arg_nm))))
  }

  # update call
  new_call <- as.call(c(call_matched[[1]], args))
  new_call <- as.call(c(quote(rlang::inject), new_call))
  new_call

  # build code
  multi_construct

  code <- styler::style_text(code)
  do.call("return", list(code), envir = sys.frame(1))
}



if (FALSE) {
  f <- function(x, ...) {
    construct_reprex()
    c(x, ...)
  }

  # f <- function(x, y) {
  #   construct_reprex()
  #   x
  # }

  x <- 3
  y <- 4
  foo <- function(...) {
    bar(...)
  }

  bar <- function(...) {
    construct_reprex()
    subset(...)
  }

  foo(cars, speed > 4)
}

