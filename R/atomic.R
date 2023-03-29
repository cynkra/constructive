#' Constructive options for atomic types
#'
#' These options will be used on atomic types ("logical", "integer", "numeric", "complex", "character" and "raw")
#'
#' @param ... Should not be used. Forces passing arguments by name.
#' @param trim `NULL` or integerish. Maximum of elements showed before it's trimmed.
#' Note that it will necessarily produce code that doesn't reproduce the input.
#' This code will parse without failure but its evaluation might fail.
#' @param fill String. Method to use to represent the trimmed elements.
#'
#' If `trim` is provided, depending on `fill` we will present trimmed elements as followed:
#' * `"default"` : Use default atomic constructors, so for instance `c("a", "b", "c")` might become `c("a", character(2))`.
#' * `"rlang"` : Use rlang atomic constructors, so for instance `c("a", "b", "c")` might become `c("a", rlang::new_character(2))`,
#'   these `rlang` constructors create vectors of `NAs`, so it's different from the default option.
#' * `"+"`: Use unary `+`, so for instance `c("a", "b", "c")` might become `c("a", +2)`.
#' * `"..."`: Use `...`, so for instance `c("a", "b", "c")` might become `c("a", ...)`
#' * `"none"`: Don't represent trimmed elements.
#'
#' @return An object of class <constructive_options/constructive_options_atomic>
#' @export
opts_atomic <- function(..., trim = NULL, fill = c("default", "rlang", "+", "...", "none")) {
  combine_errors(
    ellipsis::check_dots_empty(),
    abort_not_null_or_integerish(trim),
    fill <- rlang::arg_match(fill)
  )
  structure(
    class = c("constructive_options", "constructive_options_atomic"),
    list(trim = trim, fill = fill)
  )
}

#' @export
construct_idiomatic.atomic <- function(x, ..., one_liner = FALSE) {
  opts <- fetch_opts("atomic", ...)
  trim <- opts$trim
  fill <- opts$fill

  nms <- names(x)
  attributes(x) <- NULL
  # if all names are "" we let `repair_attributes_impl()` deal with it
  names(x) <- if (!all(nms == "")) nms

  l <- length(x)
  if (!is.null(trim) && trim < l) {
    opts$trim <- NULL
    code <- construct_idiomatic.atomic(x[seq_len(trim)], opts, ..., one_liner = one_liner)
    if (fill == "none" || trim == 0) return(code)
    if (trim == 1) code <- sprintf("c(%s)", code)
    replacement <- switch(
      fill,
      default = sprintf("%s(%s)", mode(x), l - trim),
      rlang = sprintf("rlang::new_%s(%s)", typeof(x), l - trim),
      "+" = paste0("+", l - trim),
      "..." = "..."
    )
    replacement <- sprintf(", %s)", replacement)
    code[[length(code)]] <- sub(")$", replacement, code[[length(code)]])
    return(code)
  }

  if (!is.double(x)) {
    code <- deparse(x)
    if (one_liner) code <- paste(code, collapse = " ")
    return(code)
  }

  # numeric

  # doubles need special treatment because deparse doesn't produce faithful code
  l <- length(x)
  if (l == 0) return("numeric(0)")
  # unnamed scalars don't need c()
  if (l == 1 && is.null(names(x))) return(format_flex(x, all_na = TRUE))

  args <- vapply(x, format_flex, character(1), all_na = all(is.na(x)))
  code <- construct_apply(args, "c", ..., new_line = FALSE, language = TRUE)
  if (one_liner) code <- paste(code, collapse = " ")
  code
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
  formatted <- format(x, digits = 15)
  if (formatted == "NA") {
    if (all_na) return("NA_real_") else return("NA")
  }
  if (as.numeric(formatted) == x) return(formatted)
  formatted <- format(x, digits = 22)
  if (as.numeric(formatted) == x) return(formatted)
  # remove from coverage since system dependent
  sprintf("%a", x) # nocov
}
