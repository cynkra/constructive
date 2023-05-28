# nocov start

#' @export
#' @importFrom roxygen2 roxy_tag_parse
roxy_tag_parse.roxy_tag_enumerateOptFunctions <- function(x) {
  x$raw <- "."
  roxygen2::tag_markdown(x)
}

#' @export
#' @importFrom roxygen2 roxy_tag_rd
roxy_tag_rd.roxy_tag_enumerateOptFunctions <- function(x, base_path, env) {
  roxygen2::rd_section("enumerateOptFunctions", x$val)
}

#' @export
format.rd_section_enumerateOptFunctions <- function(...) {
  fun_nms <- sort(ls(asNamespace("constructive"), pattern = "^opts_"))
  funs <- mget(fun_nms, asNamespace("constructive"))
  signatures <- mapply(construct_signature, funs, fun_nms, USE.NAMES = FALSE, MoreArgs = list(one_liner = TRUE))
  signatures <- prettycode::highlight(signatures)
  signatures <- gsub("^[^(]+(.*)", "\\1", signatures)
  signatures_formatted <- sprintf(
    "\\code{\\link[=%s]{%s}%s}",
    fun_nms,
    fun_nms,
    signatures
  )

  paste0(
    "\\section{Constructive options}{\n",
    "Constructive options provide a way to customize the output of `construct()`.\n",
    "We can provide calls to `opts_*()` functions to the `...` argument. Each of ",
    "these functions targets a specific type or class and is documented on its own page.\n\n",
    "\\itemize{\n",
    paste0("  \\item ", signatures_formatted, "\n", collapse = ""),
    "}\n",
    "}\n"
  )
}

# nocov end
