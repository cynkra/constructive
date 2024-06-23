#' Constructive options for numeric_version
#'
#' Depending on `constructor`, we construct the object as follows:
#' * `"numeric_version"` : We use `numeric_version()`
#' * `"next"` : Use the constructor for the next supported class. Call `.class2()`
#'   on the object to see in which order the methods will be tried. This will usually
#'   be equivalent to `"list"`
#' * `"list"` : We define as a list and repair attributes
#'
#' @param constructor String. Name of the function used to construct the object.
#' @inheritParams opts_atomic
#'
#' @return An object of class <constructive_options/constructive_options_numeric_version>
#' @export
opts_numeric_version  <- function(constructor = c("numeric_version", "next", "list"), ...) {
  .cstr_options("numeric_version", constructor = constructor[[1]], ...)
}

#' @export
#' @method .cstr_construct numeric_version
.cstr_construct.numeric_version <- function(x, ...) {
  opts <- list(...)$opts$numeric_version %||% opts_numeric_version()
  if (is_corrupted_numeric_version(x) || opts$constructor == "next") return(NextMethod())
  UseMethod(".cstr_construct.numeric_version", structure(NA, class = opts$constructor))
}

is_corrupted_numeric_version <- function(x) {
  # TODO
  FALSE
}

#' @export
#' @method .cstr_construct.numeric_version numeric_version
.cstr_construct.numeric_version.numeric_version <- function(x, ...) {
  code <- .cstr_apply(paste(x, collapse = "."), "numeric_version", ...)
  repair_attributes_numeric_version(x, code, ...)
}

#' @export
#' @method .cstr_construct.numeric_version list
.cstr_construct.numeric_version.list <- function(x, ...) {
  .cstr_construct.list(x, ...)
}

repair_attributes_numeric_version <- function(x, code, ...) {
  .cstr_repair_attributes(x, code, ..., idiomatic_class = "numeric_version")
}

#' Constructive options for package_version
#'
#' Depending on `constructor`, we construct the object as follows:
#' * `"package_version"` : We use `package_version()`
#' * `"next"` : Use the constructor for the next supported class. Call `.class2()`
#'   on the object to see in which order the methods will be tried. This will usually
#'   be equivalent to `"array"`
#' * `"list"` : We define as a list and repair attributes
#'
#' @param constructor String. Name of the function used to construct the object.
#' @inheritParams opts_atomic
#'
#' @return An object of class <constructive_options/constructive_options_package_version>
#' @export
opts_package_version  <- function(constructor = c("package_version", "next", "list"), ...) {
  .cstr_options("package_version", constructor = constructor[[1]], ...)
}

#' @export
#' @method .cstr_construct package_version
.cstr_construct.package_version <- function(x, ...) {
  opts <- list(...)$opts$package_version %||% opts_package_version()
  if (is_corrupted_package_version(x) || opts$constructor == "next") return(NextMethod())
  UseMethod(".cstr_construct.package_version", structure(NA, class = opts$constructor))
}

is_corrupted_package_version <- function(x) {
  # TODO
  FALSE
}

#' @export
#' @method .cstr_construct.package_version package_version
.cstr_construct.package_version.package_version <- function(x, ...) {
  code <- .cstr_apply(paste(x, collapse = "."), "package_version", ...)
  repair_attributes_package_version(x, code, ...)
}

#' @export
#' @method .cstr_construct.package_version list
.cstr_construct.package_version.list <- function(x, ...) {
  .cstr_construct.list(x, ...)
}

repair_attributes_package_version <- function(x, code, ...) {
  .cstr_repair_attributes(x, code, ..., idiomatic_class = c("package_version", "numeric_version"))
}

#' Constructive options for R_system_version
#'
#' Depending on `constructor`, we construct the object as follows:
#' * `"R_system_version"` : We use `R_system_version()`
#' * `"next"` : Use the constructor for the next supported class. Call `.class2()`
#'   on the object to see in which order the methods will be tried. This will usually
#'   be equivalent to `"list"`
#' * `"list"` : We define as a list and repair attributes
#'
#' @param constructor String. Name of the function used to construct the object.
#' @inheritParams opts_atomic
#'
#' @return An object of class <constructive_options/constructive_options_R_system_version>
#' @export
opts_R_system_version  <- function(constructor = c("R_system_version", "next", "list"), ...) {
  .cstr_options("R_system_version", constructor = constructor[[1]], ...)
}

#' @export
#' @method .cstr_construct R_system_version
.cstr_construct.R_system_version <- function(x, ...) {
  opts <- list(...)$opts$R_system_version %||% opts_R_system_version()
  if (is_corrupted_R_system_version(x) || opts$constructor == "next") return(NextMethod())
  UseMethod(".cstr_construct.R_system_version", structure(NA, class = opts$constructor))
}

is_corrupted_R_system_version <- function(x) {
  # TODO
  FALSE
}

#' @export
#' @method .cstr_construct.R_system_version R_system_version
.cstr_construct.R_system_version.R_system_version <- function(x, ...) {
  code <- .cstr_apply(paste(x, collapse = "."), "R_system_version", ...)
  repair_attributes_R_system_version(x, code, ...)
}

#' @export
#' @method .cstr_construct.R_system_version list
.cstr_construct.R_system_version.list <- function(x, ...) {
  .cstr_construct.list(x, ...)
}

repair_attributes_R_system_version <- function(x, code, ...) {
  .cstr_repair_attributes(x, code, ..., idiomatic_class = c("R_system_version", "package_version", "numeric_version"))
}
