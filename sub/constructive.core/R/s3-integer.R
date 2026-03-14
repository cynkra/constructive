#' Constructive options for type 'integer'
#'
#' @description
#' These options will be used on objects of type 'integer'. This type has
#' a single native constructor, but some additional options can be set.
#'
#' To set options on all atomic types at once see \link{opts_atomic}().
#'
#' @inheritParams opts_atomic
#' @inheritParams other-opts
#' @param fill String. Method to use to represent the trimmed elements. See `?opts_atomic`
#' @return An object of class <constructive_options/constructive_options_integer>
#' @export
opts_integer <- function(
    constructor = c("default"),
    ...,
    trim = NULL,
    fill = c("default", "rlang", "+", "...", "none"),
    compress = TRUE) {
  .cstr_combine_errors(
    abort_not_null_or_integerish(trim),
    { fill <- rlang::arg_match(fill) },
    abort_not_boolean(compress)
  )
  .cstr_options(
    "integer",
    constructor = constructor,
    ...,
    trim = trim,
    fill = fill,
    compress = compress
  )
}

#' @export
#' @method .cstr_construct integer
.cstr_construct.integer <- function(x, ...) {
  opts <- list(...)$opts$integer %||% opts_integer()
  if (is_corrupted_integer(x) || opts$constructor == "next") return(NextMethod())
  UseMethod(".cstr_construct.integer", structure(NA, class = opts$constructor))
}

is_corrupted_integer <- function(x) {
  typeof(x) != "integer"
}

#' @export
#' @method .cstr_construct.integer default
.cstr_construct.integer.default <- function(x, ...) {
  # return length 0 object early
  if (!length(x)) return(.cstr_repair_attributes(x, "integer(0)", ...))

  # we apply in priority the integer opts, fall back on atomic opts otherwise
  opts <- list(...)$opts$integer %||% opts_integer()
  x_bkp <- x

  # non standard names
  nms <- names(x)
  repair_names <- names_need_repair(nms)
  if (repair_names) names(x) <- NULL

  # trim
  # FIXME: the name repair is affected by trim
  if (!is.null(opts$trim)) {
    code <- trim_atomic(x, opts$trim, opts$fill, ...)
    if (!is.null(code)) {
      code <- .cstr_repair_attributes(x_bkp, code, ..., repair_names = repair_names)
      return(code)
    }
  }

  # compression
  if (opts$compress && is.null(names(x))) {
    code <- compress_integer(x, ...)
    if (!is.null(code)) {
      code <- .cstr_repair_attributes(x_bkp, code, ..., repair_names = repair_names)
      return(code)
    }
  }

  code <- sapply(x, deparse)
  if (!all(is.na(x))) {
    code[is.na(x)] <- "NA"
  }
  if (length(x) == 1 && is.null(names(x))) {
    code <- .cstr_repair_attributes(x_bkp, code, ..., repair_names = repair_names)
    return(code)
  }

  # wrap with c()
  code <- .cstr_apply(code, "c", ..., recurse = FALSE)
  if (list(...)$one_liner) code <- paste(code, collapse = " ")
  .cstr_repair_attributes(x_bkp, code, ..., repair_names = repair_names)
}

compress_integer <- function(x, ...) {
  l <- length(x)
  if (l > 2 && isTRUE(all(x == 0L))) return(sprintf("integer(%s)", l))
  format_rep(x, ...) %||% format_seq(x, ...)
}
