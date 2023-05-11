# FIXME: do we really need to support this class ?

constructors$constructive_options <- new.env()

#' Constructive options for the class `constructive_options`
#'
#' These options will be used on objects of class `constructive_options`.
#'
#' Depending on `constructor`, we construct the object as follows:
#' * `"opts"` : Use the relevant `constructive::opts_?()` function.
#' * `"next"` : Use the constructor for the next supported class. Call `.class2()`
#'   on the object to see in which order the methods will be tried.
#'
#' @param constructor String. Name of the function used to construct the environment, see Details section.
#' @inheritParams opts_AsIs
#' @return An object of class <constructive_options/constructive_options_array>
#' @export
opts_constructive_options <- function(constructor = c("opts", "next"), ...) {
  .cstr_combine_errors(
    constructor <- rlang::arg_match(constructor),
    ellipsis::check_dots_empty()
  )
  constructive_options("constructive_options", constructor = constructor)
}

#' @export
construct_raw.constructive_options <- function(x, ...) {
  opts <- fetch_opts("constructive_options", ...)
  if (is_corrupted_constructive_options(x) || opts$constructor == "next") return(NextMethod())
  constructor <- constructors$constructive_options[[opts$constructor]]
  constructor(x, ...)
}

#' @export
is_corrupted_constructive_options <- function(x) {
  # TODO
  FALSE
}

constructors$constructive_options$opts <- function(x, ...) {
  pattern <- "^constructive_options_(.*)$"
  suffix <- sub(pattern, "\\1", grep(pattern, class(x), value = TRUE))
  # FIXME: there should be 1 and only 1, else it's a corrupted object
  # and we should fall back on list constructor
  fun <- paste0("constructive::opts_", suffix)
  # don't name the constructor arg, and don't provide if it's the default
  constructor_pos <- which("constructor" == rlang::names2(x))
  if (length(constructor_pos)) {
    names(x)[[constructor_pos]] <- ""
    if (x[[constructor_pos]] == as.list(eval(parse(text=fun)))$constructor[[2]]) {
      x[[constructor_pos]] <- NULL
    }
  }
  code <- .cstr_apply(x, fun, ...)
  repair_attributes.constructive_options(x, code, ...)
}

#' @export
repair_attributes.constructive_options <- function(x, code, ..., pipe = "base") {
  repair_attributes_impl(
    x, code, ...,
    idiomatic_class = grep("^constructive_options", class(x), value = TRUE)
  )
}
