#' Global Options
#'
#' Set these options to tweak {constructive}'s global behavior, to set them
#' permanently you can edit your `.RProfile` (`usethis::edit_r_profile()` might help).
#'
#' * Set `options(constructive_print = <character>)` where `<character>` is a vector
#'     of strings among the following :
#'     * `"console"` : The default behavior, the code is printed in the console
#'     * `"script"` : The code is copied to a new R script
#'     * `"reprex"` : The code is shown in the viewer as a reprex,
#'     the reprex (not only the code!) is also copied to the clipboard
#'     * `"clipboard"` : The constructed code is copied to the clipboard, if combined
#'     with `"reprex"` this takes precedence
#' * Set `options(constructive_opts_template = <list>)` to set default constructive options,
#'     see documentation of the `template` arg in `?construct`
#' * Set `options(constructive_pretty = FALSE)` to disable pretty printinh using
#'     {prettycode}
#'
#' @name constructive-global_options
#' @aliases constructive_opts_template constructive_construct_to_clipboard
#'   constructive_pretty constructive_print
NULL
