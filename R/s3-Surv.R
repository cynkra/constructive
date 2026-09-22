#' Constructive options for class 'Surv'
#'
#' These options will be used on objects of class 'Surv'.
#'
#' Depending on `constructor`, we construct the object as follows:
#' * `"Surv"` (default): We build the object using `survival::Surv()` on the
#'   columns of the object. Interval censored data is built using
#'   `type = "interval2"` whenever possible, `type = "interval"` otherwise.
#'   Multi-state data is built by providing a factor to the `event` argument.
#' * `"next"` : Use the constructor for the next supported class.
#'
#' @param constructor String. Name of the function used to construct the object.
#' @inheritParams opts_atomic
#' @return An object of class <constructive_options/constructive_options_Surv>
#' @export
opts_Surv <- function(constructor = c("Surv", "next"), ...) {
  .cstr_options("Surv", constructor = constructor[[1]], ...)
}

#' @export
#' @method .cstr_construct Surv
.cstr_construct.Surv <- function(x, ...) {
  opts <- list(...)$opts$Surv %||% opts_Surv()
  if (is_corrupted_Surv(x) || opts$constructor == "next") return(NextMethod())
  UseMethod(".cstr_construct.Surv", structure(NA, class = opts$constructor))
}

is_corrupted_Surv <- function(x) {
  type <- attr(x, "type")
  if (typeof(x) != "double" || !is.character(type) || length(type) != 1) return(TRUE)
  ncol <- switch(
    type,
    right = , left = , mright = 2,
    counting = , interval = , mcounting = 3,
    return(TRUE)
  )
  dim <- attr(x, "dim")
  if (length(dim) != 2 || dim[[2]] != ncol) return(TRUE)
  m <- matrix(strip(x), ncol = ncol)
  status <- m[, ncol]
  if (type %in% c("mright", "mcounting")) {
    states <- attr(x, "states")
    if (!is.character(states) || !length(states) || anyNA(states)) return(TRUE)
    if (any(states == "") || anyDuplicated(states)) return(TRUE)
    valid_status <- c(seq(0, length(states)), NA)
  } else if (type == "interval") {
    valid_status <- c(0:3, NA)
  } else {
    valid_status <- c(0, 1, NA)
  }
  if (!all(status %in% valid_status)) return(TRUE)
  if (type == "interval") {
    is_interval <- status %in% 3
    # time2 is set to 1 when the observation is not interval censored
    if (!all(m[!is_interval, 2] %in% 1)) return(TRUE)
    # intervals with start > end are converted to NA
    if (any(m[is_interval, 1] > m[is_interval, 2], na.rm = TRUE)) return(TRUE)
  }
  if (type %in% c("counting", "mcounting")) {
    # start >= stop are converted to NA
    if (any(m[, 1] >= m[, 2], na.rm = TRUE)) return(TRUE)
  }
  FALSE
}

#' @export
#' @method .cstr_construct.Surv Surv
.cstr_construct.Surv.Surv <- function(x, ...) {
  type <- attr(x, "type")
  ncol <- attr(x, "dim")[[2]]
  m <- matrix(strip(x), ncol = ncol)
  times <- lapply(seq_len(ncol - 1), function(j) m[, j])
  status <- m[, ncol]
  input_attributes <- NULL
  if (type %in% c("mright", "mcounting")) {
    # `type = "mstate"` with a numeric event is deprecated, we always use a factor
    states <- attr(x, "states")
    levels <- attr(x, "inputAttributes")$event$levels
    if (!identical(levels[-1], states) || anyNA(levels)) {
      levels <- c(make.unique(c(states, "censor"))[[length(states) + 1]], states)
    }
    event <- factor(levels[status + 1], levels = levels)
    input_attributes <- list(event = attributes(event))
    args <- c(times, list(event))
  } else if (type == "interval") {
    is_finite_time1 <- is.finite(m[, 1])
    use_interval2 <-
      !anyNA(status) &&
      all(is_finite_time1) &&
      all(is.finite(m[status == 3, 2])) &&
      all(m[status == 3, 1] != m[status == 3, 2])
    if (use_interval2) {
      time1 <- ifelse(status == 2, NA, m[, 1])
      time2 <- ifelse(status == 0, NA, ifelse(status == 3, m[, 2], m[, 1]))
      args <- list(time1, time2, type = "interval2")
    } else {
      args <- c(times, list(status, type = "interval"))
    }
  } else {
    # logical status avoids a warning in `Surv()` when all status are NA
    event <- if (all(is.na(status))) as.logical(status) else status
    args <- c(times, list(event))
    if (type == "left") args$type <- "left"
  }
  code <- .cstr_apply(args, "survival::Surv", ...)
  repair_attributes_Surv(x, code, ..., input_attributes = input_attributes)
}

repair_attributes_Surv <- function(x, code, ..., input_attributes = NULL) {
  type <- attr(x, "type")
  ncol <- attr(x, "dim")[[2]]
  col_names <- if (ncol == 2) {
    c("time", "status")
  } else if (type %in% c("counting", "mcounting")) {
    c("start", "stop", "status")
  } else {
    c("time1", "time2", "status")
  }
  ignore <- c("dim", "type")
  if (type %in% c("mright", "mcounting")) ignore <- c(ignore, "states")
  if (identical(attr(x, "dimnames"), list(NULL, col_names))) {
    ignore <- c(ignore, "dimnames")
  }
  x_input_attributes <- attr(x, "inputAttributes")
  if (identical(x_input_attributes, input_attributes)) {
    ignore <- c(ignore, "inputAttributes")
  }
  remove <- c(
    if (is.null(attr(x, "dimnames"))) "dimnames",
    if (is.null(x_input_attributes) && !is.null(input_attributes)) "inputAttributes"
  )
  .cstr_repair_attributes(
    x, code, ...,
    idiomatic_class = "Surv",
    ignore = ignore,
    remove = remove
  )
}
