#' Display diff of object definitions
#'
#' This calls `construct()` on two objects and compares the output using
#' `diffobj::diffChr()`.
#'
#' @inheritParams construct
#' @inheritParams diffobj::diffChr
#' @param mode,interactive passed to `diffobj::diffChr()`
#' @return Returns `NULL` invisibly, called for side effects
#' @export
#'
#' @examples
#' \dontrun{
#' # some object print the same though they're different
#' # `construct_diff()` shows how they differ :
#' df1 <- data.frame(a=1, b = "x")
#' df2 <- data.frame(a=1L, b = "x", stringsAsFactors = TRUE)
#' attr(df2, "some_attribute") <- "a value"
#' df1
#' df2
#' construct_diff(df1, df2)
#'
#'
#' # Those are made easy to compare
#' construct_diff(substr, substring)
#' construct_diff(month.abb, month.name)
#'
#' # more examples borrowed from {waldo} package
#' construct_diff(c("a", "b", "c"), c("a", "B", "c"))
#' construct_diff(c("X", letters), c(letters, "X"))
#' construct_diff(list(factor("x")), list(1L))
#' construct_diff(df1, df2)
#' x <- list(a = list(b = list(c = list(structure(1, e = 1)))))
#' y <- list(a = list(b = list(c = list(structure(1, e = "a")))))
#' construct_diff(x, y)
#' }
construct_diff <- function(
    target,
    current, ...,
    data = NULL,
    pipe = NULL,
    check = TRUE,
    compare = compare_options(),
    one_liner = FALSE,
    template = getOption("constructive_opts_template"),
    classes = NULL,
    mode = c("sidebyside", "auto", "unified", "context"),
    interactive = TRUE) {
  mode <- match.arg(mode)
  tar.banner <- format_call_for_diffobj_banner(substitute(target), interactive = interactive)
  cur.banner <- format_call_for_diffobj_banner(substitute(current), interactive = interactive)
  if (identical(target, current)) {
    rlang::inform("No difference to show!")
    return(invisible(NULL))
  }
  target_code <- construct(
    target,
    ...,
    data = data,
    pipe = pipe,
    check = check,
    compare = compare,
    one_liner = one_liner,
    template = template,
    classes = classes
    )$code
  current_code <- construct(
    current,
    ...,
    data = data,
    pipe = pipe,
    check = check,
    compare = compare,
    one_liner = one_liner,
    template = template,
    classes = classes
  )$code
  f <- tempfile(fileext = ".html")
  diffobj::diffChr(
    target_code,
    current_code,
    mode = mode,
    tar.banner = tar.banner,
    cur.banner = cur.banner,
    pager = list(file.path = f),
    interactive = interactive
  )
}

format_call_for_diffobj_banner <- function(call, interactive) {
  deparsed <- rlang::expr_deparse(call)
  if (!interactive) return(paste(deparsed, collapse = " "))
  multiline <- paste(deparsed, collapse = "<BR>")
  idented <- gsub(" ", "&#x00A0;", multiline)
  idented
}
