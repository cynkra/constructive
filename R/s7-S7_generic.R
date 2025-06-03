#' Constructive options for class 'S7_generic'
#'
#' These options will be used on objects of class 'S7_generic'.
#'
#' Depending on `constructor`, we construct the object as follows:
#' * `"new_generic"` (default): We build the object using `S7::new_generic()`.
#' * `"next"` : Use the constructor for the next supported class.
#'
#' @param constructor String. Name of the function used to construct the object.
#' @param ... Additional options used by user defined constructors through the `opts` object
#' @return An object of class <constructive_options/constructive_options_S7_generic>
#' @export
opts_S7_generic <- function(constructor = c("new_generic", "next"), ...) {
  constructive::.cstr_options("S7_generic", constructor = constructor[[1]], ...)
}

#' @exportS3Method constructive::.cstr_construct
.cstr_construct.S7_generic <- function(x, ...) {
  opts <- list(...)$opts$S7_generic %||% opts_S7_generic()
  if (is_corrupted_S7_generic(x) || opts$constructor == "next") return(NextMethod())
  UseMethod(".cstr_construct.S7_generic", structure(NA, class = opts$constructor))
}

is_corrupted_S7_generic <- function(x) {
  FALSE
}

#' @export
.cstr_construct.S7_generic.new_generic <- function(x, ...) {
  # opts <- list(...)$opts$S7_generic %||% opts_S7_generic()
  # FIXME: this doesn't account for the fun arg, but I don't understand yet
  # where this function ends up
  args <- list(
    attr(x, "name"),
    dispatch_args = attr(x, "dispatch_args")
  )
  if (!identical(body(x), quote(S7::S7_dispatch()))) {
    args$fun <- strip(x)
    attr(args$fun, "srcref") <- attr(x, "srcref")
  }
  code <- constructive::.cstr_apply(args, fun = "S7::new_generic", ...)

  # construct methods
  methods <- attr(x, "methods")
  for (nm in names(methods)) {
    method <- methods[[nm]]
    class <- attr(method, "signature")[[1]]
    method <- strip(method)
    method_code <- .cstr_apply(list(class, value = method), "(S7::`method<-`)")
    code <- .cstr_pipe(code, method_code)
  }


  constructive::.cstr_repair_attributes(
    x, code, ...,
    ignore = c(
      "name", "dispatch_args", "S7_class", "parent", "properties",
      "constructor", "methods", "srcref"
    ),
    idiomatic_class = c("S7_generic", "function", "S7_object")
  )
}
