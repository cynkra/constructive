#' Constructive options for class 'ggplot2::labels'
#'
#' These options will be used on objects of class 'ggplot2::labels'.
#'
#' Depending on `constructor`, we construct the object as follows:
#' * `"labs"` (default): We build the object using `ggplot2::labs()`.
#' * `"next"` : Use the constructor for the next supported class.
#'
#' @param constructor String. Name of the function used to construct the object.
#' @param ... Additional options used by user defined constructors through the `opts` object
#' @return An object of class <constructive_options/constructive_options_ggplot2::labels>
#' @export
opts_ggplot2_labels <- function(constructor = c("labs", "next"), ...) {
  constructive::.cstr_options("ggplot2::labels", constructor = constructor[[1]], ...)
}

#' @exportS3Method constructive::.cstr_construct
`.cstr_construct.ggplot2::labels` <- function(x, ...) {
  opts <- list(...)$opts$`ggplot2::labels` %||% opts_ggplot2_labels()
  if (`is_corrupted_ggplot2::labels`(x) || opts$constructor == "next") return(NextMethod())
  UseMethod(".cstr_construct.ggplot2::labels", structure(NA, class = opts$constructor))
}

`is_corrupted_ggplot2::labels` <- function(x) {
  FALSE
}

#' @export
`.cstr_construct.ggplot2::labels.labs` <- function(x, ...) {
  # opts <- list(...)$opts$ggplot2::labels %||% opts_ggplot2::labels()
  args <- strip(x)
  code <- constructive::.cstr_apply(args, fun = "ggplot2::labs", ...)
  constructive::.cstr_repair_attributes(
    x, code, ...,
    ignore = "S7_class",
    idiomatic_class = c("ggplot2::labels", "gg", "S7_object")
  )
}
