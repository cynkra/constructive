#' @export
#' @rdname other-opts
opts_double <- function(
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
    "double",
    constructor = constructor,
    ...,
    trim = trim,
    fill = fill,
    compress = compress
  )
}

#' @export
#' @method .cstr_construct double
.cstr_construct.double <- function(x, ...) {
  opts <- list(...)$opts$double %||% opts_double()
  if (is_corrupted_double(x) || opts$constructor == "next") return(NextMethod())
  UseMethod(".cstr_construct.double", structure(NA, class = opts$constructor))
}

is_corrupted_double <- function(x) {
  typeof(x) != "double"
}

#' @export
#' @method .cstr_construct.double default
.cstr_construct.double.default <- function(x, ...) {
  # return length 0 object early
  if (!length(x)) return(.cstr_repair_attributes(x, "numeric(0)", ...))

  # we apply in priority the double opts, fall back on atomic opts otherwise
  opts <- list(...)$opts$double %||% opts_double()
  x_bkp <- x

  # non standard names
  nms <- names(x)
  names_need_repair <- !is.null(nms) && (anyNA(nms) || all(nms == ""))
  if (names_need_repair) names(x) <- NULL

  # trim
  # FIXME: the name reparation is affected by trim
  if (!is.null(opts$trim)) {
    code <- trim_atomic(x, opts$trim, opts$fill, ...)
    if (!is.null(code)) {
      code <- .cstr_repair_attributes(x_bkp, code, ...)
      return(code)
    }
  }

  # compression
  if (opts$compress && is.null(names(x))) {
    code <- compress_double(x, ...)
    if (!is.null(code)) {
      code <- .cstr_repair_attributes(x_bkp, code, ...)
      return(code)
    }
  }

  if (length(x) == 1 && is.null(names(x))) {
    code <- format_flex(x, all_na = TRUE)
    code <- .cstr_repair_attributes(x_bkp, code, ...)
    return(code)
  }

  code <- vapply(x, format_flex, character(1), all_na = all(is_na_real(x)))

  # wrap with c()
  code <- .cstr_apply(code, "c", ..., recurse = FALSE)
  if (list(...)$one_liner) code <- paste(code, collapse = " ")
  .cstr_repair_attributes(x_bkp, code, ...)
}

compress_double <- function(x, ...) {
  l <- length(x)
  if (l > 2 && isTRUE(all(x == 0L))) {
    signs <- sign(1/x)
    if (all(signs == 1)) return(sprintf("numeric(%s)", l))
    if (all(signs == -1)) return(sprintf("-numeric(%s)", l))
  }
  # don't compress if x contains both positive and negative zeroes
  zeros_ind <- which(x == 0)
  if (length(zeros_ind)) {
    contains_pos_and_neg_zeroes <- length(unique(1/x[zeros_ind])) != 1
    if (contains_pos_and_neg_zeroes) return(NULL)
  }
  format_rep(x, ..., double = TRUE) %||% format_seq(x, ...)
}

format_flex <- function(x, all_na) {
  # negative zeroes
  if (identical(x, 0) && sign(1/x) == -1) return("-0")
  # negative NAs, commented for now as might be overkill, and inconsistent
  # if(is.na(x) && serialize(x, NULL)[[32]] == as.raw(0xff)) {
  #   if (is.nan(x)) return("-NaN")
  #   return("-NA_real_")
  # }
  formatted <- format.default(x, digits = 15)
  if (formatted == "NA") {
    if (all_na) return("NA_real_") else return("NA")
  }
  if (formatted == "NaN") {
    return("NaN")
  }
  if (as.numeric(formatted) == x) return(formatted)
  # FIXME: Increase digits only for those array elements that don't match
  for (digits in 16:22) {
    formatted <- format.default(x, digits = digits)
    if (as.numeric(formatted) == x) return(formatted)
  }
  # remove from coverage since system dependent
  # (similarly to .deparseOpts("hexNumeric"))
  sprintf("%a", x) # nocov
}
