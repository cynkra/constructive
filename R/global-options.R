#' Global Options
#'
#' Set these options to tweak {constructive}'s global behavior, to set them
#' permanently you can edit your `.RProfile` (`usethis::edit_r_profile()` might help).
#'
#' * Set `options(constructive_construct_to_clipboard = TRUE)` to copy the output
#'     to the clipboard when printing a "constructive" object (e.g. when calling
#'     `construct()` without assignment).
#' * Set `options(constructive_print = FALSE)` not to print constructive objects to
#'     the console at all (this probably only makes sense with
#'     `options(constructive_construct_to_clipboard = TRUE)`)
#' * Set `options(constructive_opts_template = <list>)` to set default constructive options,
#'     see documentation of the `template` arg in `?construct`
#' * Set `options(constructive_pretty = FALSE)` to disable pretty printinh using
#'     {prettycode}
#'
#' @name constructive-global_options
#' @aliases constructive_opts_template constructive_construct_to_clipboard
#'   constructive_pretty constructive_print
NULL
