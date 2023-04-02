#' @export
construct_idiomatic.numeric_version <- function(x, ...) {
  construct_apply(paste(x, collapse = "."), "numeric_version", ...)
}

#' @export
repair_attributes.numeric_version <- function(x, code, ...) {
  repair_attributes_impl(x, code, ..., idiomatic_class = "numeric_version")
}

#' @export
construct_idiomatic.package_version <- function(x, ...) {
  construct_apply(paste(x, collapse = "."), "package_version", ...)
}

#' @export
repair_attributes.package_version <- function(x, code, ...) {
  repair_attributes_impl(x, code, ..., idiomatic_class = c("package_version", "numeric_version"))
}

#' @export
construct_idiomatic.R_system_version <- function(x, ...) {
  construct_apply(paste(x, collapse = "."), "R_system_version", ...)
}

#' @export
repair_attributes.R_system_version <- function(x, code, ...) {
  repair_attributes_impl(x, code, ..., idiomatic_class = c("R_system_version", "package_version", "numeric_version"))
}


