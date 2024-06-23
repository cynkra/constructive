#' Constructive options for class 'S4'
#'
#' These options will be used on objects of class 'S4'. Note that the support
#' for S4 is very experimental so might easily break. Please report issues if it
#' does.
#'
#' @param constructor String. Name of the function used to construct the object, see Details section.
#' @inheritParams opts_atomic
#' @return An object of class <constructive_options/constructive_options_S4>
#' @export
opts_S4 <- function(constructor = c("new"), ...) {
  .cstr_options("S4", constructor = constructor[[1]], ...)
}

#' @export
#' @method .cstr_construct S4
.cstr_construct.S4 <- function(x, ...) {
  opts <- list(...)$opts$S4 %||% opts_S4()
  if (is_corrupted_S4(x) || opts$constructor == "next") return(NextMethod())
  UseMethod(".cstr_construct.S4", structure(NA, class = opts$constructor))
}

is_corrupted_S4 <- function(x) {
  !isS4(x)
}

#' @export
#' @method .cstr_construct.S4 new
.cstr_construct.S4.new <- function(x, env, ...) {
  cl <- class(x)
  if (
    attr(cl, "package") == environmentName(env) ||
    (identical(env, .GlobalEnv) && attr(cl, "package") == ".GlobalEnv")) {
    attr(cl, "package") <- NULL
  }
  slots <- getSlots(cl)
  nms <- names(slots)
  if (".Data" %in% nms) {
    attrs <- attributes(x)[setdiff(nms, ".Data")]
    args <- c(list(cl), .Data = x@.Data, attrs)
  } else {
    attrs <- attributes(x)[nms]
    args <- c(list(cl), attrs)
  }
  code <- .cstr_apply(args, fun = "new", env = env, ...)
  repair_attributes_S4(x, code, env = env, ...)
}

repair_attributes_S4 <- function(x, code, ..., selfref = FALSE) {
  .cstr_repair_attributes(
    x, code, ...,
    ignore = names(getSlots(class(x))),
    idiomatic_class = class(x),
    flag_s4 = FALSE
  )
}

