#' Constructive options for class 'S7_class_ref'
#'
#' These options will be used on objects of class 'S7_class_ref'. Since S7
#' 0.2.2.9000 an S7 object stores its class in the "_S7_class" attribute through
#' such a reference, which `S7::new_class()` creates and stores in the
#' environment of the class.
#'
#' Depending on `constructor`, we construct the object as follows:
#' * `"environment"` (default): We build the class and fetch the reference
#'   from its environment.
#' * `"next"` : Use the constructor for the next supported class.
#'
#' @param constructor String. Name of the function used to construct the object.
#' @param ... Additional options used by user defined constructors through the `opts` object
#' @return An object of class <constructive_options/constructive_options_S7_class_ref>
#' @export
opts_S7_class_ref <- function(constructor = c("environment", "next"), ...) {
  constructive::.cstr_options("S7_class_ref", constructor = constructor[[1]], ...)
}

#' @exportS3Method constructive::.cstr_construct
.cstr_construct.S7_class_ref <- function(x, ...) {
  opts <- list(...)$opts$S7_class_ref %||% opts_S7_class_ref()
  if (is_corrupted_S7_class_ref(x) || opts$constructor == "next") return(NextMethod())
  UseMethod(".cstr_construct.S7_class_ref", structure(NA, class = opts$constructor))
}

is_corrupted_S7_class_ref <- function(x) {
  # S7 <= 0.2.2 doesn't use class references
  if (with_versions(S7 <= "0.2.2")) return(TRUE)
  if (!is.environment(x)) return(TRUE)
  class <- get0("class", envir = x, inherits = FALSE)
  if (!inherits(class, "S7_class")) return(TRUE)
  ref <- get0(".S7_class_ref", envir = environment(class), inherits = FALSE)
  !identical(ref, x)
}

#' @export
.cstr_construct.S7_class_ref.environment <- function(x, ...) {
  # opts <- list(...)$opts$S7_class_ref %||% opts_S7_class_ref()
  code <- .cstr_apply(list(get("class", envir = x)), "environment", ...)
  code[[length(code)]] <- paste0(code[[length(code)]], "$.S7_class_ref")
  constructive::.cstr_repair_attributes(
    x, code, ...,
    idiomatic_class = "S7_class_ref"
  )
}
