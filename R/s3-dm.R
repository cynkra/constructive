constructors$dm <- new.env()

#' Constructive options class 'dm'
#'
#' These options will be used on objects of class 'dm'.
#'
#' Depending on `constructor`, we construct the environment as follows:
#' * `"dm"` (default): We use `dm::dm()` and other functions from {dm} to adjust the content.
#' * `"next"` : Use the constructor for the next supported class. Call `.class2()`
#'   on the object to see in which order the methods will be tried.
#' * `"list"` : Use `list()` and treat the class as a regular attribute.
#'
#' @param constructor String. Name of the function used to construct the environment.
#' @inheritParams opts_atomic
#' @param origin Origin to be used, ignored when irrelevant.
#'
#' @return An object of class <constructive_options/constructive_options_environment>
#' @export
opts_dm <- function(constructor = c("dm", "next", "list"), ...) {
  .cstr_combine_errors(
    constructor <- rlang::arg_match(constructor),
    ellipsis::check_dots_empty()
  )
  .cstr_options("dm", constructor = constructor)
}
#' @export
construct_raw.dm <- function(x, ...) {
  opts <- .cstr_fetch_opts("dm", ...)
  if (is_corrupted_dm(x) || opts$constructor == "next") return(NextMethod())
  constructor <- constructors$dm[[opts$constructor]]
  constructor(x, ...)
}

is_corrupted_dm <- function(x) {
  # TODO
  FALSE
}

constructors$dm$dm <- function(x, ..., one_liner, pipe) {
  def <- unclass(x)$def
  named_list_of_tables <- set_names(def$data, def$table)
  code <- .cstr_apply(
    named_list_of_tables, fun = "dm::dm", keep_trailing_comma = TRUE, implicit_names = TRUE, one_liner = one_liner, pipe = pipe, ...)

  pk_code <- unlist(Map(
    function(table, pk_tibble) {
      if (!nrow(pk_tibble)) return(character())
      column_code <- construct_raw(pk_tibble$column[[1]], pipe = pipe, one_liner = one_liner, ...)
      paste0("dm::dm_add_pk(", protect(table), ", ", paste(column_code, collapse = "\n") , ")")
    } ,
    def$table,
    def$pks,
    USE.NAMES = FALSE))
  if (length(pk_code)) {
    pk_code <- paste(pk_code, collapse = "|>\n")
    code <- pipe(code, pk_code, pipe, one_liner)
  }

  fk_code <- unlist(Map(
    function(ref_table, fk_tibble) {
      if (!nrow(fk_tibble)) return(character())
      Map(function(table, column, ref_column) {
        paste0(
          "dm::dm_add_fk(",
          protect(table), ", ",
          paste(construct_raw(column, pipe = pipe, one_liner = one_liner, ...), collapse = "\n"), ",",
          protect(ref_table), ", ",
          paste(construct_raw(ref_column, pipe = pipe, one_liner = one_liner, ...), collapse = "\n"), ")"
        )
      },
      fk_tibble$table,
      fk_tibble$column,
      fk_tibble$ref_column
      )
    } ,
    def$table,
    def$fks,
    USE.NAMES = FALSE))
  if (length(fk_code)) {
    fk_code <- paste(fk_code, collapse = "|>\n")
    code <- pipe(code, fk_code, pipe, one_liner)
  }

  colors <- set_names(def$table, def$display)[!is.na(def$display)]
  if (length(colors)) {
    color_code <- .cstr_apply(colors, "dm::dm_set_colors", pipe = pipe, one_liner = one_liner, ...)
    code <- pipe(code, color_code, pipe, one_liner)
  }

  repair_attributes.dm(x, code, ..., one_liner = one_liner, pipe = pipe)
}

constructors$dm$list <- function(x, ...) {
  construct_raw.list(x, ...)
}

#' @export
repair_attributes.dm <- function(x, code, ..., pipe ="base") {
  repair_attributes_impl(
    x, code, ...,
    pipe = pipe,
    idiomatic_class = "dm",
    ignore = c("version")
  )
}
