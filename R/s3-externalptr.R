#' @export
construct_idiomatic.externalptr <- function(x, ...) {
  sprintf('constructive::external_pointer("%s")', external_pointer_address(x))
}


