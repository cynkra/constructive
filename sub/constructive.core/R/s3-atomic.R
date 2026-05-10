#' Constructive options for atomic types
#'
#' These options will be used on atomic types ("logical", "integer", "numeric", "complex", "character" and "raw").
#' They can also be directly provided to atomic types through their own `opts_*()`
#' function, and in this case the latter will have precedence.
#'
#' @param ... Additional options used by user defined constructors through the `opts` object
#' @param trim `NULL` or integerish. Maximum of elements showed before it's trimmed.
#' Note that it will necessarily produce code that doesn't reproduce the input.
#' This code will parse without failure but its evaluation might fail.
#' @param fill String. Method to use to represent the trimmed elements.
#' @param compress Boolean. If `TRUE` instead of `c()` Use `seq()`, `rep()`
#'   when relevant to simplify the output.
#'
#' @details
#'
#' If `trim` is provided, depending on `fill` we will present trimmed elements as followed:
#' * `"default"` : Use default atomic constructors, so for instance `c("a", "b", "c")` might become `c("a", character(2))`.
#' * `"rlang"` : Use rlang atomic constructors, so for instance `c("a", "b", "c")` might become `c("a", rlang::new_character(2))`,
#'   these `rlang` constructors create vectors of `NAs`, so it's different from the default option.
#' * `"+"`: Use unary `+`, so for instance `c("a", "b", "c")` might become `c("a", +2)`.
#' * `"..."`: Use `...`, so for instance `c("a", "b", "c")` might become `c("a", ...)`
#' * `"none"`: Don't represent trimmed elements.
#'
#' Depending on the case some or all of the choices above might generate code that
#' cannot be executed. The 2 former options above are the most likely to succeed
#' and produce an output of the same type and dimensions recursively. This would
#' at least be the case for data frame.
#'
#' @return An object of class <constructive_options/constructive_options_atomic>
#' @export
#' @examples
#' construct(iris, opts_atomic(trim = 2), check = FALSE) # fill = "default"
#' construct(iris, opts_atomic(trim = 2, fill = "rlang"), check = FALSE)
#' construct(iris, opts_atomic(trim = 2, fill = "+"), check = FALSE)
#' construct(iris, opts_atomic(trim = 2, fill = "..."), check = FALSE)
#' construct(iris, opts_atomic(trim = 2, fill = "none"), check = FALSE)
#' construct(iris, opts_atomic(trim = 2, fill = "none"), check = FALSE)
#' x <- c("a a", "a\U000000A0a", "a\U00002002a", "\U430 \U430")
opts_atomic <- function(
    ...,
    trim = NULL,
    fill = c("default", "rlang", "+", "...", "none"),
    compress = TRUE
) {
  .cstr_combine_errors(
    abort_not_null_or_integerish(trim),
    fill <- rlang::arg_match(fill),
    abort_not_boolean(compress)
  )
  if (any(c("unicode_representation", "escape") %in% names(list(...)))) {
    msg <- "`unicode_representation` and `escape` are deprecated in `opts_atomic()`"
    info1 <-  "Set those in `opts_character()` instead for the same effect"
    info2 <- "Set those directly in the main function (e.g. `construct()`) to apply them on both character vectors, symbols and argument names"
    rlang::warn(c(msg, i = info1, i = info2))
  }

  .cstr_options("atomic", ..., trim = trim, fill = fill, compress = compress) #, unicode_representation = unicode_representation, escape = escape)
}

# divisors except self and 1
divisors <- function(x) {
  y <- setdiff(seq_len(x / 2), 1)
  y[x %% y == 0]
}

# A rle without checks that treats NAs like a regular values and return an unnamed list
# with value first
rle2 <- function (x, double = FALSE) {
  n <- length(x)
  t <- x[-1L]
  h <- x[-n]
  y <- t != h
  if (double) {
    y <- ifelse(
      is.na(y),
      !(is.nan(t) & is.nan(h)) & !(is_na_real(t) & is_na_real(h)),
      y
    )
  } else {
    y <- ifelse(is.na(y), !(is.na(t) & is.na(h)), y)
  }
  i <- c(which(y), n)
  list(x[i], diff(c(0L, i)))
}


# Special treatmnent of doubles is necessary because `dput()`, used for default
# the method, sometimes cuts values too short,
# however this gives ugly values in the general case
# in the 2 following cases we want the shortest "equal" output
# format(5.1, digits = 22) # "5.099999999999999644729"
# format(1e24, digits = 22) # 999999999999999983222784
# => so we use `digits = 16` for the default since it seems to simplify those values,
#    and we fall back on `digits = 22` for other cases
# if it still doesn't fit it we use `sprintf("%a", x)` which provides a representation
# that is not very readable but always works


format_flex <- function(x, all_na) {
  # negative zeroes
  if (identical(x, 0) && sign(1/x) == -1) return("-0")
  # negative NAs, commented for now as might be overkill, and inconsistent
  # if(is.na(x) && serialize(x, NULL)[[32]] == as.raw(0xff)) {
  #   if (is.nan(x)) return("-NaN")
  #   return("-NA_real_")
  # }
  formatted <- format(x, digits = 15)
  if (formatted == "NA") {
    if (all_na) return("NA_real_") else return("NA")
  }
  if (formatted == "NaN") {
    return("NaN")
  }
  if (as.numeric(formatted) == x) return(formatted)
  # FIXME: Increase digits only for those array elements that don't match
  for (digits in 16:22) {
    formatted <- format(x, digits = digits)
    if (as.numeric(formatted) == x) return(formatted)
  }
  # remove from coverage since system dependent
  # (similarly to .deparseOpts("hexNumeric"))
  sprintf("%a", x) # nocov
}
