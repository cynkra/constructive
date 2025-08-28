#' Constructive options for class 'S4'
#'
#' These options will be used on objects of class 'S4'. 
#'
#' Depending on `constructor`, we construct the object as follows:
#' * `"new"` (default): We build the function using `new()` if possible.
#'   If the class has a "initialize" method we have no practical way to 
#'   reverse-engineer the inputs so we fall back to the "prototype" constructor
#' * `"prototype"` : We start from `getClass("S4")@prototype` and add attributes.
#'
#' @param constructor String. Name of the function used to construct the object, see Details section.
#' @inheritParams opts_atomic
#' @return An object of class <constructive_options/constructive_options_S4>
#' @export
opts_S4 <- function(constructor = c("new", "prototype"), ...) {
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

  class_has_initialize <- !is.null(methods::getMethod("initialize", cl, optional = TRUE))

  if (class_has_initialize) {
    return(.cstr_construct.S4.prototype(x, env = env, ...))
  }

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
  .cstr_repair_attributes(
    x, code, env = env, ...,
    ignore = names(getSlots(class(x))),
    idiomatic_class = class(x),
    flag_s4 = FALSE
  )
}

#' @export
.cstr_construct.S4.prototype <- function(x, ...) {
  code <- "getClass(\"S4\")@prototype"
  .cstr_repair_attributes(
    x, code, ...,
    flag_s4 = FALSE
  )
}

