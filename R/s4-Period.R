#' Constructive options for class 'Period'
#'
#' These options will be used on objects of class 'Period'.
#'
#' Depending on `constructor`, we construct the object as follows:
#' * `"period"` (default): We build the object using `lubridate::period()`,
#'   providing the `years`, `months`, `days`, `hours`, `minutes` and `seconds`
#'   arguments, omitting the components that are zero for all elements.
#' * `"next"` : Use the constructor for the next supported class.
#'
#' @param constructor String. Name of the function used to construct the object, see Details section.
#' @inheritParams opts_atomic
#' @return An object of class <constructive_options/constructive_options_Period>
#' @export
opts_Period <- function(constructor = c("period", "next"), ...) {
  .cstr_options("Period", constructor = constructor[[1]], ...)
}

#' @export
#' @method .cstr_construct Period
.cstr_construct.Period <- function(x, ...) {
  opts <- list(...)$opts$Period %||% opts_Period()
  if (is_corrupted_Period(x) || opts$constructor == "next") return(NextMethod())
  UseMethod(".cstr_construct.Period", structure(NA, class = opts$constructor))
}

is_corrupted_Period <- function(x) {
  if (!isS4(x) || typeof(x) != "double") return(TRUE)
  components <- list(x@year, x@month, x@day, x@hour, x@minute)
  !all(vapply(components, typeof, character(1)) == "double") ||
    !all(lengths(components) == length(x@.Data)) ||
    # `lubridate::period()` fails on non integer components other than seconds
    any(unlist(components) %% 1 != 0, na.rm = TRUE)
}

#' @export
#' @method .cstr_construct.Period period
.cstr_construct.Period.period <- function(x, ...) {
  args <- list(
    years = x@year,
    months = x@month,
    days = x@day,
    hours = x@hour,
    minutes = x@minute,
    seconds = x@.Data
  )
  if (length(x@.Data)) {
    all_zero <- vapply(args, function(arg) isTRUE(all(arg == 0)), logical(1))
    # keep `seconds` if all components are zero
    all_zero[["seconds"]] <- all_zero[["seconds"]] && !all(all_zero)
    args <- args[!all_zero]
  } else {
    args <- list()
  }
  code <- .cstr_apply(args, "lubridate::period", ...)
  repair_attributes_Period(x, code, ...)
}

repair_attributes_Period <- function(x, code, ...) {
  .cstr_repair_attributes(
    x, code, ...,
    ignore = names(methods::getSlots(class(x))),
    idiomatic_class = class(x),
    flag_s4 = FALSE
  )
}
