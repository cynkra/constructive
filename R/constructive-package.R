globals <- new.env()

# FIXME: find a way to support extensions better
globals$ggpackages <- "ggplot2"

#' @keywords internal
"_PACKAGE"

#' @import rlang
#' @importFrom utils capture.output head tail getFromNamespace
#'   packageDescription methods
#' @importFrom stats setNames
#' @importFrom methods getSlots
#' @importFrom grDevices pdf dev.off
#' @useDynLib
NULL

## usethis namespace: start
## usethis namespace: end
NULL
