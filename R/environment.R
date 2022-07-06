#' @export
construct_idiomatic.environment <- function(x, env_as_list = TRUE, ...) {
  # this is not very robust but might help in some useful special cases

  # The name of `asNamespace("pkg")` is always "pkg" and print as `<environment: namespace:pkg>`
  # The name of `as.environment("package:pkg")` is ALMOST always "package:pkg" and prints as
  #  `<environment: package:pkg>` + attributes
  # The exception is `as.environment("package:base")` which prints as
  #   `<environment: base>` and whose name is "base"
  # This means `asNamespace("base")` (a.k.a. `.BaseNamespaceEnv`) and
  #   `as.environment("package:base")` (a.k.a. `baseenv()`) have the same name
  #   but are different. So we implement a workaround.
  if(identical(x, baseenv())) return('baseenv()')
  name <- environmentName(x)
  if (name == "R_GlobalEnv") return(".GlobalEnv")
  if (name == "base") return(".BaseNamespaceEnv")
  if (name %in% row.names(installed.packages())) return(sprintf('asNamespace("%s")', name))
  if (name %in% search()) return(sprintf('as.environment("%s")', name))
  if (env_as_list) {
    wrap(construct_apply(as.list(x), ...), "as.environment", new_line = FALSE)
  } else {
    "new.env()"
  }
}

#' @export
repair_attributes.environment <- function(x, code, pipe ="base", ...) {
  repair_attributes_impl(
    x, code, pipe,
    ignore = c("name", "path"),
    ...
  )
}
