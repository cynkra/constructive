#' Global Options
#'
#' Set these options to tweak {constructive}'s global behavior, to set them
#' permanently you can edit your `.RProfile` (`usethis::edit_r_profile()` might help).
#'
#' * Set `options(constructive_print = <character>)` where `<character>` is a vector
#'     of strings among `"console"`, `"clipboard"` and `"reprex"`. The default
#'     behavior is "console". If you use `"clipboard"`, with or without `"reprex"`,
#'     the clipboard will contain only the code. If you use `"reprex"` without
#'     `"clipboard"` the clipboard will still be used to receive the reprex,
#'     can be tweaked with `options(reprex.clipboard = FALSE)`
#' * Set `options(constructive_opts_template = <list>)` to set default constructive options,
#'     see documentation of the `template` arg in `?construct`
#' * Set `options(constructive_pretty = FALSE)` to disable pretty printinh using
#'     {prettycode}
#'
#' @name constructive-global_options
#' @aliases constructive_opts_template constructive_construct_to_clipboard
#'   constructive_pretty constructive_print
NULL
