#' Construct to clipboard
#'
#' This is a simple wrapper for convenience, `construct_clip(x, ...)` is equivalent to
#' `print(construct(x, ...), print_mode = "clipboard")` (an idiom that you might
#' use to use the clipboard with other functions). For more flexible printing
#' options see `?constructive_print_mode`.
#'
#' @inheritParams construct
#'
#' @return An object of class 'constructive', invisibly. Called for side effects.
#' @export
#'
#' @examples
#' \dontrun{
#' construct_clip(head(cars))
#' }
construct_clip <- function(
    x,
    ...,
    data = NULL,
    pipe = NULL,
    check = NULL,
    unicode_representation = c("ascii", "latin", "character", "unicode"),
    escape = FALSE,
    pedantic_encoding = FALSE,
    compare = compare_options(), one_liner = FALSE,
    template = getOption("constructive_opts_template"),
    classes = NULL) {
  out <- construct(
    x,
    ...,
    data = data,
    pipe = pipe,
    check = check,
    unicode_representation = unicode_representation,
    escape = escape,
    pedantic_encoding = pedantic_encoding,
    compare = compare,
    template = template,
    classes = classes
  )
  print(out, print_mode = "clipboard")
}
