#' @export
construct_idiomatic.ggproto <- function(x, ggproto.ignore_draw_key = FALSE, ggproto.simplify_to_string = FALSE, ...) {

  ## Find an exact match (with or without draw_key)
  # if we find we return early

  if (ggproto.ignore_draw_key) {
    x <- as.list(x)
    x$draw_key <- NULL
  }

  for (pkg in globals$ggpackages) {
    pkg_protos <- Filter(function(x) inherits(x, "ggproto"), as.list(asNamespace(pkg)))
    for (nm in names(pkg_protos)) {
      proto <- pkg_protos[[nm]]
      if (ggproto.ignore_draw_key) {
        proto <- as.list(proto)
        proto$draw_key <- NULL
      }
      if(identical(x, proto, ignore.environment = TRUE)) {
        if (ggproto.simplify_to_string && pkg == "ggplot2") {
          # FIXME: not good enough GeomContourFixed -> countour_fixed ?
          #return(sub("[A-Z][a-z]+([A-Z])([a-z]+)", '"\\L\\1\\2"', nm, perl = TRUE))
          return(sprintf('"%s"', sub("^[^_]+_", "", ggplot2:::snakeize(nm), nm)))
        }
        return(paste0(pkg, ":::", nm))
      }
    }
  }

  ## Find matches that use the same functions (with or without draw_key)
  # might be refactored into "get_super()"

  # maybe add Coord, Facet, see ?ggplot2::Stat, but since they're not customizable (?)
  # I think they will be handled above
  # do another pass in packages where we check if we find
  # a similar object to inherit from, containing the same functions, since that's
  # what's ugly when printing, it should find ggbeeswarm::PositionBeeSwarm and edit
  # it to make it really compact (and that should work pretty much all the time)
  args <- as.list(x)
  strip_proto <- function(x) {
    x <- Filter(is.function, x)
    #x[["palette"]] <- NULL
    x
  }
  filtered_args <- strip_proto(args)
  inherits_from <- NA

  # for debugging
  #browser()
  if (FALSE) {
    proto <- ScaleDiscretePosition
    proto_stripped <- Filter(is.function, as.list(proto))
    waldo::compare(proto_stripped, filtered_args)
  }

  for (pkg in globals$ggpackages) {
    pkg_protos <- Filter(function(x) inherits(x, "ggproto"), as.list(asNamespace(pkg)))
    identical_functions <- function(proto, filtered_args) {
      proto_list <- as.list(proto)
      if (ggproto.ignore_draw_key) proto_list[["draw_key"]] <- NULL
      identical(strip_proto(proto_list), filtered_args, ignore.environment = TRUE)
    }
    ind_lgl <- vapply(pkg_protos, identical_functions, filtered_args, FUN.VALUE = logical(1))
    if(any(ind_lgl)) {
      inherits_from <- names(pkg_protos)[ind_lgl][[1]] # [[1]] in case several identical objects
      inherits_from <- setdiff(inherits_from, c("Stat", "Geom", "Position", "Scale"))
      break
    }
  }

  ## If not found inherit from one of the 4 simple classes
  # not sure if this part is robust yet

  if (is.na(inherits_from)) {
    pkg <- "ggplot2"
    inherits_from <- intersect(class(x), c("Stat", "Geom", "Position", "Scale"))
  }
  class = sprintf('"%s"', setdiff(class(x), c("ggproto", "gg", inherits_from)))
  if(!length(class)) class <- '""'
  ggproto0 <- getFromNamespace(inherits_from, pkg)
  inherits_from <- paste0(pkg, ":::", inherits_from)

  ## set arguments, skipping those who are the same as defaults
  for(nm in names(args)) {
    if (identical(ggproto0[[nm]], x[[nm]], ignore.environment = TRUE)) {
      args[[nm]] <- NULL
    }
  }
  # Why do we do this ?
  args <-lapply(args, `attributes<-`, NULL)
  args_chr <- c(
    list(class),
    list(inherits_from),
    lapply(args, construct_raw, ggproto.ignore_draw_key = ggproto.ignore_draw_key, ggproto.simplify_to_string = ggproto.simplify_to_string, ...)
    )
  construct_apply(args_chr, "ggplot2::ggproto", language = TRUE, ggproto.ignore_draw_key = ggproto.ignore_draw_key, ggproto.simplify_to_string = ggproto.simplify_to_string, ...)
}

#' @export
repair_attributes.ggproto <- function(x, code, pipe = "base", ...) {
  repair_attributes_impl(
    x, code, pipe,
    ignore = c("class", "srcref"),
    ...
  )
}
