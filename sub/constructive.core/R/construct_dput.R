#' Construct using only low level constructors
#'
#' * `construct_dput()` is a closer counterpart to `base::dput()` that doesn't
#'   use higher level constructors such as `data.frame()` and `factor()`.
#' * `construct_base()` uses higher constructors, but only for the classes
#'   maintained in the default base R packages. This includes `data.frame()`
#'   and `factor()`, the S4 constructors from the 'method' package etc,
#'   but not `data.table()` and other constructors for classes from other
#'   packages.
#'
#' Both functions are valuable for object inspection, and might provide more
#' stable snapshots, since supporting more classes in the package means
#' the default output of `construct()` might change over time for some objects.
#'
#' To use higher level constructor from the base package itself, excluding
#' for instance `stats::ts()`, `utils::person()` or
#' `methods::classGeneratorFunction()`), we can call `construct(x, classes = "{base}"`
#'
#' @inheritParams construct
#'
#' @return An object of class 'constructive'.
#' @export
#'
#' @examples
#' construct_dput(head(iris, 2))
#' construct_base(head(iris, 2))
construct_dput <- function(
    x,
    ...,
    data = NULL,
    pipe = NULL,
    check = NULL,
    unicode_representation = c("ascii", "latin", "character", "unicode"),
    escape = FALSE,
    pedantic_encoding = FALSE,
    compare = compare_options(), one_liner = FALSE,
    template = getOption("constructive_opts_template")) {
  construct(
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
    classes = "*none*"
  )
}

#' @export
#' @rdname construct_dput
construct_base <- function(
    x,
    ...,
    data = NULL,
    pipe = NULL,
    check = NULL,
    unicode_representation = c("ascii", "latin", "character", "unicode"),
    escape = FALSE,
    pedantic_encoding = FALSE,
    compare = compare_options(), one_liner = FALSE,
    template = getOption("constructive_opts_template")) {
  construct(
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
    classes = "*base*"
  )
}
