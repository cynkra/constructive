#' Constructive options for class 'hms'
#'
#' These options will be used on objects of class 'hms'.
#'
#' Depending on `constructor`, we construct the object as follows:
#' * `"as_hms"` (default): We build the object using `hms::as_hms()` on a
#'   character vector in the format `"12:34:56"`. If some values cannot be
#'   reproduced exactly this way (e.g. negative values, durations of more than
#'   24 hours, or some fractional seconds) we fall back to the `"hms"` constructor.
#' * `"hms"` : We build the object using `hms::hms()` on `seconds`, `minutes`
#'   and `hours` arguments. If the decomposition is not exact we provide
#'   only the `seconds` argument.
#' * `"new_hms"` : We build the object using `hms::new_hms()` on a double vector.
#' * `"next"` : Use the constructor for the next supported class. Call `.class2()`
#'   on the object to see in which order the methods will be tried.
#' * `"double"` : We define as an atomic vector and repair attributes.
#'
#' @param constructor String. Name of the function used to construct the object, see Details section.
#' @inheritParams opts_atomic
#'
#' @return An object of class <constructive_options/constructive_options_hms>
#' @export
opts_hms <- function(constructor = c("as_hms", "hms", "new_hms", "next", "double"), ...) {
  .cstr_options("hms", constructor = constructor[[1]], ...)
}

#' @export
#' @method .cstr_construct hms
.cstr_construct.hms <- function(x, ...) {
  opts <- list(...)$opts$hms %||% opts_hms()
  if (is_corrupted_hms(x) || opts$constructor == "next") return(NextMethod())
  UseMethod(".cstr_construct.hms", structure(NA, class = opts$constructor))
}

is_corrupted_hms <- function(x) {
  !is.double(x) || !identical(attr(x, "units"), "secs")
}

#' @export
#' @method .cstr_construct.hms as_hms
.cstr_construct.hms.as_hms <- function(x, ...) {
  x_dbl <- x
  attributes(x_dbl) <- NULL
  if (all(is.na(x_dbl)) || !all(is.na(x_dbl) | (x_dbl >= 0 & x_dbl <= 86400))) {
    return(.cstr_construct.hms.hms(x, ...))
  }
  hours <- x_dbl %/% 3600
  minutes <- x_dbl %% 3600 %/% 60
  seconds <- as.character(x_dbl %% 60)
  seconds <- ifelse(x_dbl %% 60 < 10, paste0("0", seconds), seconds)
  chr <- sprintf("%02d:%02d:%s", hours, minutes, seconds)
  chr[is.na(x_dbl)] <- NA
  parsed <- suppressWarnings(hms::parse_hms(chr))
  attributes(parsed) <- NULL
  if (!identical(parsed, x_dbl)) {
    return(.cstr_construct.hms.hms(x, ...))
  }
  code <- .cstr_apply(list(chr), "hms::as_hms", ..., new_line = FALSE)
  repair_attributes_hms(x, code, ..., repair_names = TRUE)
}

#' @export
#' @method .cstr_construct.hms hms
.cstr_construct.hms.hms <- function(x, ...) {
  x_dbl <- x
  attributes(x_dbl) <- NULL
  # adding 0 turns negative zeros into zeros
  hours <- trunc(x_dbl / 3600) + 0
  minutes <- trunc((x_dbl - hours * 3600) / 60) + 0
  seconds <- x_dbl - hours * 3600 - minutes * 60
  args <- list(seconds = seconds, minutes = minutes, hours = hours)
  if (!identical(seconds + minutes * 60 + hours * 3600, x_dbl)) {
    args <- list(seconds = x_dbl)
  }
  # hms::hms() requires contiguous arguments, we drop all zero args at the edges
  used <- which(vapply(args, function(arg) any(arg != 0, na.rm = TRUE), logical(1)))
  if (length(used)) args <- args[min(used):max(used)] else args <- args[1]
  if (!length(x_dbl)) args <- list()
  code <- .cstr_apply(args, "hms::hms", ...)
  repair_attributes_hms(x, code, ..., repair_names = TRUE)
}

#' @export
#' @method .cstr_construct.hms new_hms
.cstr_construct.hms.new_hms <- function(x, ...) {
  x_dbl <- x
  attributes(x_dbl) <- attributes(x)["names"]
  code <- .cstr_apply(list(x_dbl), "hms::new_hms", ..., new_line = FALSE)
  repair_attributes_hms(x, code, ...)
}

#' @export
#' @method .cstr_construct.hms double
.cstr_construct.hms.double <- function(x, ...) {
  .cstr_construct.double(x, ...)
}

repair_attributes_hms <- function(x, code, ..., repair_names = FALSE) {
  .cstr_repair_attributes(
    x, code, ...,
    idiomatic_class = c("hms", "difftime"),
    ignore = "units",
    repair_names = repair_names
  )
}
