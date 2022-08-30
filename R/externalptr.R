#' @export
construct_idiomatic.externalptr <- function(x, ...) {
  rlang::warn(
    sprintf("{constructive} cannot reconstruct pointers, %s was replaced by `NULL`", capture.output(x))
  )
  "NULL"
}
