#' Constructive options for class 'CLASS'
#'
#' These options will be used on objects of class 'CLASS'.
#'
#' Depending on `constructor`, we construct the object as follows:
#' * `"CONSTRUCTOR"` (default): We build the object using `CONSTRUCTOR()`.
#' * `"next"` : Use the constructor for the next supported class.
#'
#' @param constructor String. Name of the function used to construct the object.
#' @return An object of class <constructive_options/constructive_options_CLASS>
#' @export
opts_CLASS <- function(constructor = c("CONSTRUCTOR", "next"), ...) {
  # what's forwarded through `...`will be accessible through the `opts`
  # object in the methods.
  # you might add arguments to the function, to document those options,
  # don't forget to forward them below as well
  .cstr_options("CLASS", constructor = constructor[[1]], ...)
}

#' @export
#' @method .cstr_construct CLASS
.cstr_construct.CLASS <- function(x, ...) {
  # there is probably no need for you to modify this function at all
  opts <- list(...)$opts$CLASS %||% opts_CLASS()
  if (is_corrupted_CLASS(x) || opts$constructor == "next") return(NextMethod())
  # This odd looking code dispatches to a method based on the name of
  # the constructor rather than the class
  UseMethod(".cstr_construct.CLASS", structure(NA, class = opts$constructor))
}

is_corrupted_CLASS <- function(x) {
  # check here if the object has the right structure to be constructed
  # leaving FALSE is fine but you'll be vulnerable to corrupted objects
  FALSE
}

#' @export
#' @method .cstr_construct.CLASS CONSTRUCTOR
.cstr_construct.CLASS.CONSTRUCTOR <- function(x, ...) {
  # can be removed if your constructor doesn't use additional options
  opts <- list(...)$opts$CLASS %||% opts_CLASS()
  args <- ...
  # This creates a call CONSTRUCTOR(...) where ... is the constructed code
  # of the arguments stored in `args`
  # Sometimes we want to construct the code of the args separately, and
  # use `recurse = FALSE` below
  code <- .cstr_apply(args, fun = "CONSTRUCTOR", ...)
  # attribute reparation is strictly speaking constructor specific but in
  # most cases it's the same within a class
  repair_attributes_CLASS(x, code, ...)
}

repair_attributes_CLASS <- function(x, code, ...) {
  .cstr_repair_attributes(
    x, code, ...,
    # not necessarily just a string, but the whole class(x) vector
    idiomatic_class = c("CLASS")
  )
}
