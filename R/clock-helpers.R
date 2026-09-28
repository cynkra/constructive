# Shared helpers for the {clock} classes, the "precision" attribute of clock
# objects is an index into this vector, starting at 0
clock_precisions <- c(
  "year", "quarter", "month", "week", "day", "hour", "minute", "second",
  "millisecond", "microsecond", "nanosecond"
)

is_corrupted_clock_precision <- function(precision, valid = 0:10) {
  !is.integer(precision) || length(precision) != 1 || !precision %in% valid
}

# `fields` are the fields of the calendar at the finest precision, `start` the
# valid values of the "start" attribute for calendars that have one
is_corrupted_clock_calendar <- function(x, fields, start = NULL) {
  precision <- attr(x, "precision")
  if (typeof(x) != "list" || is_corrupted_clock_precision(precision)) return(TRUE)
  field <- if (precision >= 8) "subsecond" else clock_precisions[[precision + 1]]
  n <- match(field, fields)
  if (is.na(n)) return(TRUE)
  # year_month_weekday stores the weekday in "day" and its index in "index"
  if (identical(fields[n + 1], "index")) n <- n + 1
  if (!identical(attr(x, "names"), fields[seq_len(n)])) return(TRUE)
  if (!all(vapply(x, is.integer, logical(1)))) return(TRUE)
  if (length(unique(lengths(x))) > 1) return(TRUE)
  !is.null(start) && is_corrupted_clock_precision(attr(x, "start"), start)
}

construct_clock_calendar <- function(x, fun, ...) {
  args <- unname(lapply(x, as.double))
  # recycle constant fields, unless all are constant since they define the length
  constant <- vapply(args, function(arg) length(unique(arg)) == 1, logical(1))
  if (!all(constant)) args[constant] <- lapply(args[constant], `[`, 1)
  start <- attr(x, "start")
  # the default start is 1 for both year_week_day() and year_quarter_day()
  if (!is.null(start) && start != 1) args$start <- as.double(start)
  precision <- attr(x, "precision")
  if (precision >= 8) args$subsecond_precision <- clock_precisions[[precision + 1]]
  .cstr_apply(args, fun, ...)
}

# durations, time points and zoned times store 64 bit integers in 2 doubles
is_corrupted_clock_int64 <- function(x, precision = 0:10) {
  typeof(x) != "list" ||
    !identical(attr(x, "names"), c("lower", "upper")) ||
    !all(vapply(x, is.double, logical(1))) ||
    length(unique(lengths(x))) > 1 ||
    is_corrupted_clock_precision(attr(x, "precision"), precision)
}

# time points are built from a year_month_day calendar
construct_clock_time_point <- function(x, fun, ...) {
  .cstr_apply(list(clock::as_year_month_day(x)), fun, ...)
}
