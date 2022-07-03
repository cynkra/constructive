#' Build code to recreate an object
#'
#' @param x An object
#'
#' @param data named list of data we don't want to deparse
#' @param check Boolean. Whether to check if the created code reproduces the object
#'   exactly (using `identical()`)
#' @param pipe Which pipe to use, either "base" or "magrittr"
#' @param check should we try to eval the result and check if it is identical to
#'   `data`
#' @param ... Additional parameters passed to `construct_impl()` generic and methods.
#'
#' @export
construct <- function(x, data = NULL, pipe = c("base", "magrittr"), check = TRUE, ...) {
  pipe <- match.arg(pipe)
  data <- preprocess_data(data)
  code <- try_construct(x, data, pipe = pipe, ...)
  styled_code <- try_parse(code, data)
  if (check) {
    evaled <- try_eval(styled_code, data)
    check_round_trip(x, evaled, styled_code)
  }
  styled_code
}

# helpers for the above --------------------------------------------------------

preprocess_data <- function(data) {
  if(is.character(data)) data <- namespace_as_list(data)
  if (is.environment(data)) data <- as.list(data)
  data
}

try_construct <- function(x, data, ...) {
  caller <- caller_env()

  rlang::try_fetch(construct_raw(x, data = data, ...), error = function(e) {
    abort("{constructive} could not build the requested code.", parent = e, call = caller)
  })
}

try_parse <- function(code, data) {
  caller <- caller_env()
  rlang::try_fetch(
    styler::style_text(code, scope = "line_breaks"),
    error = function(e) {
      abort("The code built by {constructive} could not be parsed.", parent = e, call = caller)
    }
  )
}

try_eval <- function(styled_code, data) {
  caller <- caller_env()
  rlang::try_fetch(
    eval(parse(text = styled_code), envir = data, enclos = caller),
    error = function(e) {
      print(styled_code)
      abort("The code built by {constructive} could not be evaluated.", parent = e, call = caller)
    }
  )
}

check_round_trip <- function(x, evaled, styled_code) {
  caller <- caller_env()
  # FIXME -> check_round_trip
  if (!identical(x, evaled)) {
    print(styled_code)
    abort(
      c(
        paste0(
          "{constructive} couldn't create code that reproduces perfectly the output\n",
          paste(waldo::compare(x, evaled, x_arg = "original", y_arg = "recreated"), collapse = "\n")
        ),
        i = "use `check = FALSE` to ignore this error"
      ),
      call = caller
    )
  }
  invisible(NULL)
}


