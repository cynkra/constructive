construct_repair <- function(target, ref) {
  calls <- deparse_call(substitute(ref))
  calls <- c(calls, rec(target, ref, template = NULL))
  code <- paste(calls, collapse = "%>%\n")
  styler::style_text(code)
  #writeLines(code)
}

#TODO

rec <- function(target, ref, ind = NULL, ...) {

  # remove or add at top level
  l_target <- length(target)
  l_ref <-  length(ref)
  attrs_ref <- attributes(ref)
  attrs_target <- attributes(target)
  attrs_ref$names <- NULL
  attrs_target$names <- NULL
  n_common <- min(l_target, l_ref)
  calls <- character()

  # FIXME: we might make this a unlist(sapply()) once we return rather than print
  for (i in seq_len(n_common)) {
    if (!identical(target[[i]], ref[[i]])) {
      if (!is.list(target[[i]]) || !is.list(ref[[i]])) {
        # FIXME: deal with the case where only attributes need to be repaired

        ind_code <- construct_raw(c(ind, i), ...)
        target_code <- construct_raw(target[[i]], ...)
        target_code <- paste0("~", target_code)
        calls <- c(calls, construct_apply(list(ind_code, target_code), "purrr::modify_in", language = TRUE, ...))
      } else {
        calls <- c(calls, rec(target[[i]], ref[[i]] , c(ind, i), ...))
      }
    }
  }

  if (l_ref > l_target) {
    # remove
    for (i in (l_target + 1):l_ref) {
      ind_code <- construct_raw(c(ind, i), ...)
      target_code <- "~NULL"
      calls <- c(calls, construct_apply(list(ind_code, target_code), "purrr::modify_in", language = TRUE, ...))
    }
  }

  if (l_target > l_ref) {
    for (i in (l_ref + 1):l_target) {
      if (is.null(ind)) {
        calls <- c(calls, construct_apply(list(target[i]), "append", ...))
      } else {
        # ind_code <- construct_raw(c(ind, i), ...)
        # target_code <- construct_raw(target[[i]], ...)
        calls <- c(calls, construct_apply(list(c(ind, i), "append", target[i]), "purrr::modify_in", ...))
      }
    }
    if(!identical(names(ref), names(target)[seq_along(ref)])) {
      if (is.null(ind)) {
        calls <- c(calls, construct_apply(list(names(target)[seq_along(ref)]), "setNames", ...))
      } else {
        calls <- c(calls, construct_apply(list(ind, "setNames", names(target)[seq_along(ref)]), "purrr::modify_in", ...))
      }
    }
  }

  if (!identical(attrs_ref, attrs_target)) {
    # this is not correct and possibly redundant
    #  we migt have to remove attributes
    #  no need to redefine attrs that are already correct
    if (is.null(ind)) {
      calls <- c(calls, construct_apply(list(attrs_target), "structure", ...))
    } else {
      # ind_code <- construct_raw(c(ind, i), ...)
      # target_code <- construct_raw(target[[i]], ...)
      calls <- c(calls, construct_apply(list(c(ind, i), "structure", attrs_target), "purrr::modify_in", ...))
    }
  }

  calls
}

# x <- list(a=1, b= list(u =2, v = 3))
# y <- list(a=1, b= data.frame(u =2, v = 3))
# y <-  list(a=1, b= list(u =2, v = factor("c", levels = letters[1:3])))
# y <-  list(a=1)
# y <-  list(a=1, b = 3, c = )
#
#
# construct_repair(x, y)
