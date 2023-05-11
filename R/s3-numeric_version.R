constructors$numeric_version <- new.env()

#' Constructive options for numeric_version
#'
#' Depending on `constructor`, we construct the environment as follows:
#' * `"numeric_version"` : We use `numeric_version()`
#' * `"next"` : Use the constructor for the next supported class. Call `.class2()`
#'   on the object to see in which order the methods will be tried. This will usually
#'   be equivalent to `"array"`
#' * `"atomic"` : We define as an atomic vector and repair attributes
#'
#' @param constructor String. Name of the function used to construct the environment.
#' @inheritParams opts_atomic
#' @param origin Origin to be used, ignored when irrelevant.
#'
#' @return An object of class <constructive_options/constructive_options_environment>
#' @export
opts_numeric_version  <- function(constructor = c("numeric_version", "next", "atomic"), ...) {
  .cstr_combine_errors(
    constructor <- rlang::arg_match(constructor),
    ellipsis::check_dots_empty()
  )
  constructive_options("numeric_version", constructor = constructor)
}

#' @export
construct_raw.numeric_version <- function(x, ...) {
  opts <- .cstr_fetch_opts("numeric_version", ...)
  if (is_corrupted_numeric_version(x) || opts$constructor == "next") return(NextMethod())
  constructors$numeric_version[[opts$constructor]](x, ...)
}

is_corrupted_numeric_version <- function(x) {
  # TODO
  FALSE
}

constructors$numeric_version$numeric_version <- function(x, ...) {
  code <- .cstr_apply(paste(x, collapse = "."), "numeric_version", ...)
  repair_attributes.numeric_version(x, code, ...)
}

constructors$numeric_version$atomic <- function(x, ...) {
  construct_raw.atomic(x, ...)
}

#' @export
repair_attributes.numeric_version <- function(x, code, ...) {
  repair_attributes_impl(x, code, ..., idiomatic_class = "numeric_version")
}

constructors$package_version <- new.env()

#' Constructive options for package_version
#'
#' Depending on `constructor`, we construct the environment as follows:
#' * `"package_version"` : We use `package_version()`
#' * `"next"` : Use the constructor for the next supported class. Call `.class2()`
#'   on the object to see in which order the methods will be tried. This will usually
#'   be equivalent to `"array"`
#' * `"atomic"` : We define as an atomic vector and repair attributes
#'
#' @param constructor String. Name of the function used to construct the environment.
#' @inheritParams opts_atomic
#' @param origin Origin to be used, ignored when irrelevant.
#'
#' @return An object of class <constructive_options/constructive_options_environment>
#' @export
opts_package_version  <- function(constructor = c("package_version", "next", "atomic"), ...) {
  .cstr_combine_errors(
    constructor <- rlang::arg_match(constructor),
    ellipsis::check_dots_empty()
  )
  constructive_options("package_version", constructor = constructor)
}

#' @export
construct_raw.package_version <- function(x, ...) {
  opts <- .cstr_fetch_opts("package_version", ...)
  if (is_corrupted_package_version(x) || opts$constructor == "next") return(NextMethod())
  constructors$package_version[[opts$constructor]](x, ...)
}

is_corrupted_package_version <- function(x) {
  # TODO
  FALSE
}

constructors$package_version$package_version <- function(x, ...) {
  code <- .cstr_apply(paste(x, collapse = "."), "package_version", ...)
  repair_attributes.package_version(x, code, ...)
}

constructors$package_version$atomic <- function(x, ...) {
  construct_raw.atomic(x, ...)
}

#' @export
repair_attributes.package_version <- function(x, code, ...) {
  repair_attributes_impl(x, code, ..., idiomatic_class = c("package_version", "numeric_version"))
}

constructors$R_system_version <- new.env()

#' Constructive options for R_system_version
#'
#' Depending on `constructor`, we construct the environment as follows:
#' * `"R_system_version"` : We use `R_system_version()`
#' * `"next"` : Use the constructor for the next supported class. Call `.class2()`
#'   on the object to see in which order the methods will be tried. This will usually
#'   be equivalent to `"array"`
#' * `"atomic"` : We define as an atomic vector and repair attributes
#'
#' @param constructor String. Name of the function used to construct the environment.
#' @inheritParams opts_atomic
#' @param origin Origin to be used, ignored when irrelevant.
#'
#' @return An object of class <constructive_options/constructive_options_environment>
#' @export
opts_R_system_version  <- function(constructor = c("R_system_version", "next", "atomic"), ...) {
  .cstr_combine_errors(
    constructor <- rlang::arg_match(constructor),
    ellipsis::check_dots_empty()
  )
  constructive_options("R_system_version", constructor = constructor)
}

#' @export
construct_raw.R_system_version <- function(x, ...) {
  opts <- .cstr_fetch_opts("R_system_version", ...)
  if (is_corrupted_R_system_version(x) || opts$constructor == "next") return(NextMethod())
  constructors$R_system_version[[opts$constructor]](x, ...)
}

is_corrupted_R_system_version <- function(x) {
  # TODO
  FALSE
}

constructors$R_system_version$R_system_version <- function(x, ...) {
  code <- .cstr_apply(paste(x, collapse = "."), "R_system_version", ...)
  repair_attributes.R_system_version(x, code, ...)
}

constructors$R_system_version$atomic <- function(x, ...) {
  construct_raw.atomic(x, ...)
}

#' @export
repair_attributes.R_system_version <- function(x, code, ...) {
  repair_attributes_impl(x, code, ..., idiomatic_class = c("R_system_version", "package_version", "numeric_version"))
}


