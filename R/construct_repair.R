# FIXME: append() scrapes the class and attributes
#   we might use an append replacement depending on the class
#   or we just reconstruct all attrs after append(), but this might not be very minimal

construct_repair <- function(target, ref) {
  ref_code <- deparse_call(substitute(ref))
  calls <- rec(target, ref, template = NULL)
  code <- pipe(ref_code, calls, pipe = "magrittr", one_liner = FALSE)
  code[[length(code)]] <- sub(" %>%$", "", code[[length(code)]])
  styler::style_text(code)
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
        call <- construct_apply(list(ind_code, target_code), "purrr::modify_in", language = TRUE, ...)
        calls <- c(calls, pipe(call, NULL, pipe = "magrittr", one_liner = FALSE))
      } else {
        call <- rec(target[[i]], ref[[i]] , c(ind, i), ...)
        calls <- c(call, calls)
      }
    }
  }

  if (l_ref > l_target) {
    # remove
    for (i in (l_target + 1):l_ref) {
      ind_code <- construct_raw(c(ind, i), ...)
      target_code <- "~NULL"
      calls <- construct_apply(list(ind_code, target_code), "purrr::modify_in", language = TRUE, ...)
      calls <- c(calls, pipe(call, NULL, pipe = "magrittr", one_liner = FALSE))
    }
  }

  if (l_target > l_ref) {
    for (i in (l_ref + 1):l_target) {
      if (is.null(ind)) {
        call <- construct_apply(list(target[i]), "append", ...)
      } else {
        # ind_code <- construct_raw(c(ind, i), ...)
        # target_code <- construct_raw(target[[i]], ...)
        call <- construct_apply(list(c(ind, i), "append", target[i]), "purrr::modify_in", ...)
      }
      calls <- c(calls, pipe(call, NULL, pipe = "magrittr", one_liner = FALSE))
    }
    if(!identical(names(ref), names(target)[seq_along(ref)])) {
      if (is.null(ind)) {
        call <- construct_apply(list(names(target)[seq_along(ref)]), "setNames", ...)
      } else {
        call <- construct_apply(list(ind, "setNames", names(target)[seq_along(ref)]), "purrr::modify_in", ...)
      }
      calls <- c(calls, pipe(call, NULL, pipe = "magrittr", one_liner = FALSE))
    }
  }

  if (!identical(attrs_ref, attrs_target)) {
    # we loop through the args and ignore as long as they are the same and named identically
    for (i in seq_along(attrs_target)) {
      if (identical(names(attrs_target)[[1]], names(attrs_ref)[[1]]) &&
          identical(attrs_target[[i]], attrs_ref[[i]])) {
        attrs_target[[i]] <- NULL
        attrs_ref[[i]] <- NULL
      } else {
        break
      }
    }
    # attributes to remove from ref
    if (length(attrs_ref)) {
      attrs_ref[] <- list(NULL)
    }
    attrs <- c(attrs_ref, attrs_target)

    if (is.null(ind)) {
      call <- construct_apply(attrs, "structure", ...)
    } else {
      call <- construct_apply(c(list(ind), "structure", attrs), "purrr::modify_in", ...)
    }
    calls <- c(calls, pipe(call, NULL, pipe = "magrittr", one_liner = FALSE))
  }

  calls
}

# foo <- list(1)
# attributes(foo) <- list(a1=1,a2=2)
# bar <- list(1)
# attributes(bar) <- list(a1=1,a3=2)
# x <- list(foo)
# y <- list(bar)
# construct_repair(x, y)
# x <- list(a=1, b= list(u =2, v = 3))
# y <- list(a=1, b= data.frame(u =2, v = 3))
# y <-  list(a=1, b= list(u =2, v = factor("c", levels = letters[1:3])))
# y <-  list(a=1)
# y <-  list(a=1, b = 3, c = )
#
#
# construct_repair(x, y)
