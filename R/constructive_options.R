#' @export
construct_idiomatic.constructive_options <- function(x, ...) {
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
  construct_apply(x, fun, ...)
}

#' @export
repair_attributes.constructive_options <- function(x, code, ..., pipe = "base") {
  repair_attributes_impl(
    x, code, ...,
    idiomatic_class = grep("^constructive_options", class(x), value = TRUE)
  )
}
