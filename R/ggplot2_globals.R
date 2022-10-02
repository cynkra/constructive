globals <- new.env()
globals$ggpackages <- "ggplot2"

add_gg_packages <- function(packages) {
  # FIXME check input type and package existence
  globals$ggpackages <- union(globals$ggpackages, packages)
}
