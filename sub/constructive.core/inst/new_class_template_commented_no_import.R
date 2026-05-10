#' Constructive options for class '.CLASS1.'
#'
#' These options will be used on objects of class '.CLASS1.'.
#'
#' Depending on `constructor`, we construct the object as follows:
#' * `".CONSTRUCTOR."` (default): We build the object using `.PKG::CONSTRUCTOR.()`.
#' * `"next"` : Use the constructor for the next supported class.
#'
#' @param constructor String. Name of the function used to construct the object.
#' @param ... Additional options used by user defined constructors through the `opts` object
#' @return An object of class <constructive_options/constructive_options_.CLASS1.>
#' @export
opts_.CLASS1. <- function(constructor = c(".CONSTRUCTOR.", "next"), ...) {
  # What's forwarded through `...`will be accessible through the `opts`
  # object in the methods.
  # You might add arguments to the function, to document those options,
  # don't forget to forward them below as well
  constructive::.cstr_options(".CLASS1.", constructor = constructor[[1]], ...)
}

#' @exportS3Method constructive::.cstr_construct
.cstr_construct..CLASS1. <- function(x, ...) {
  # There is probably no need for you to modify this function at all
  opts <- list(...)$opts$.CLASS1. %||% opts_.CLASS1.()
  if (is_corrupted_.CLASS1.(x) || opts$constructor == "next") return(NextMethod())
  # This odd looking code dispatches to a method based on the name of
  # the constructor rather than the class
  UseMethod(".cstr_construct..CLASS1.", structure(NA, class = opts$constructor))
}

is_corrupted_.CLASS1. <- function(x) {
  # check here if the object has the right structure to be constructed
  # leaving FALSE is fine but you'll be vulnerable to corrupted objects
  FALSE
}

#' @export
.cstr_construct..CLASS1...CONSTRUCTOR. <- function(x, ...) {
  # If needed, fetch additional options fed through opts_.CLASS1.()
  # opts <- list(...)$opts$.CLASS1. %||% opts_.CLASS1.()

  # Instead of the call below we need to fetch the args of the constructor in `x`.
  args <- list()

  # This creates a call .CONSTRUCTOR.(...) where ... is the constructed code
  # of the arguments stored in `args`
  # Sometimes we want to construct the code of the args separately, i.e. store
  # code rather than objects in `args`, and use `recurse = FALSE` below
  code <- constructive::.cstr_apply(args, fun = ".PKG::CONSTRUCTOR.", ...)

  # .cstr_repair_attributes() makes sure that attributes that are not built
  # by the idiomatic constructor are generated
  constructive::.cstr_repair_attributes(
    x, code, ...,
    # attributes built by the constructor
    # ignore =,

    # not necessarily just a string, but the whole class(x) vector
    idiomatic_class = .CLASS.
  )
}
