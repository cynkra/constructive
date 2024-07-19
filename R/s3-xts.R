#' Constructive options for class 'xts'
#'
#' These options will be used on objects of class 'xts'.
#'
#' Depending on `constructor`, we construct the object as follows:
#' * `"as.xts.matrix"` (default): We build the object using `xts::as.xts.matrix()`.
#' * `"as.xts.data.frame"`: We build the object using `xts::as.xts.data.frame()`,
#'   this is probably the most readable option but couldn't be made the default
#'   constructor because it requires the 'xts' package to be installed .
#' * `"xts"`: We build the object using `xts::xts()`.
#' * `".xts"`: We build the object using `xts::.xts()`.
#' * `"next"`: Use the constructor for the next supported class.
#'
#' @param constructor String. Name of the function used to construct the object.
#' @param ... Additional options used by user defined constructors through the `opts` object
#' @return An object of class <constructive_options/constructive_options_xts>
#' @export
opts_xts <- function(constructor = c("as.xts.matrix", "next"), ...) {
  .cstr_options("xts", constructor = constructor[[1]], ...)
}

#' @export
#' @method .cstr_construct xts
.cstr_construct.xts <- function(x, ...) {
  opts <- list(...)$opts$xts %||% opts_xts()
  if (is_corrupted_xts(x) || opts$constructor == "next") return(NextMethod())
  UseMethod(".cstr_construct.xts", structure(NA, class = opts$constructor))
}

is_corrupted_xts <- function(x) {
  if (!typeof(x) %in% c("integer", "double")) return(TRUE)
  if (length(dim(x)) != 2) return(TRUE)
  dn <- dimnames(x)
  dimnames_are_corrupted <-
    length(dn) != 2 ||
    !is.null(dn[[1]]) ||
    !is.character(dn[[2]]) ||
    length(dn[[2]]) != dim(x)[[2]]
  if (dimnames_are_corrupted) return(TRUE)
  index <- attr(x, "index")
  index_is_corrupted <-
    !is.double(index) ||
    !is.character(attr(index, "tzone")) ||
    !is.character(attr(index, "tclass"))
  if (index_is_corrupted) return(TRUE)
  FALSE
}

#' @export
#' @method .cstr_construct.xts as.xts.matrix
.cstr_construct.xts.as.xts.matrix <- function(x, ...) {
  dimnames_ <- dimnames(x)
  dimnames_[[1]] <- as.character(as.POSIXct(
    attr(x,"index"),
    tz = attr(attr(x,"index"), "tzone"),
    # for compat with R < 4.3.0
    origin = "1970-01-01"
  ))
  args <- list(
    structure(strip(x), dim = dim(x), dimnames = dimnames_)
  )

  code <- .cstr_apply(args, fun = "xts::as.xts", ..., new_line = FALSE)
  .cstr_repair_attributes(
    x, code, ...,
    ignore = c("dim", "dimnames", "index"),
    idiomatic_class = c("xts", "zoo")
  )
}

#' @export
#' @method .cstr_construct.xts as.xts.data.frame
.cstr_construct.xts.as.xts.data.frame <- function(x, ...) {
  # we need xts for the as.data.frame()
  check_installed("xts")
  loadNamespace("xts")
  args <- list(
    as.data.frame(x)
  )

  code <- .cstr_apply(args, fun = "xts::as.xts", ..., new_line = FALSE)
  .cstr_repair_attributes(
    x, code, ...,
    ignore = c("dim", "dimnames", "index"),
    idiomatic_class = c("xts", "zoo")
  )
}

#' @export
#' @method .cstr_construct.xts .xts
.cstr_construct.xts..xts <- function(x, ...) {
  if (list(...)$one_liner) {
    args <- list(
      structure(strip(x), dim = dim(x), dimnames = dimnames(x)),
      index = attr(x, "index")
    )
    code <- .cstr_apply(args, fun = "xts::.xts", ...)
  } else {
    args <- list(
      index = attr(x, "index")
    )
    code <- .cstr_pipe(
      .cstr_construct(
        structure(strip(x), dim = dim(x), dimnames = dimnames(x))
      ),
      .cstr_apply(args, fun = "xts::.xts", ...),
      ...
    )
  }

  .cstr_repair_attributes(
    x, code, ...,
    ignore = c("dim", "dimnames", "index"),
    idiomatic_class = c("xts", "zoo")
  )
}

#' @export
#' @method .cstr_construct.xts xts
.cstr_construct.xts.xts <- function(x, ...) {

  if (list(...)$one_liner) {
    args <- list(
      structure(strip(x), dim = dim(x), dimnames = dimnames(x)),
      order.by = as.POSIXct(
        attr(x,"index"),
        tz = attr(attr(x,"index"), "tzone"),
        # for compat with R < 4.3.0
        origin = "1970-01-01"
      )
    )
    code <- .cstr_apply(args, fun = "xts::xts", ...)
  } else {
    args <- list(
      order.by = as.POSIXct(
        attr(x,"index"),
        tz = attr(attr(x,"index"), "tzone"),
        # for compat with R < 4.3.0
        origin = "1970-01-01"
      )
    )
    code <- .cstr_pipe(
      .cstr_construct(
        structure(strip(x), dim = dim(x), dimnames = dimnames(x))
      ),
      .cstr_apply(args, fun = "xts::xts", ...),
      ...
    )
  }
  .cstr_repair_attributes(
    x, code, ...,
    ignore = c("dim", "dimnames", "index"),
    idiomatic_class = c("xts", "zoo")
  )
}
