#' Constructive options class 'dm'
#'
#' These options will be used on objects of class 'dm'.
#'
#' Depending on `constructor`, we construct the object as follows:
#' * `"dm"` (default): We use `dm::dm()` and other functions from \pkg{dm} to adjust the content.
#' * `"next"` : Use the constructor for the next supported class. Call `.class2()`
#'   on the object to see in which order the methods will be tried.
#' * `"list"` : Use `list()` and treat the class as a regular attribute.
#'
#' @param constructor String. Name of the function used to construct the object.
#' @inheritParams opts_atomic
#'
#' @return An object of class <constructive_options/constructive_options_dm>
#' @export
opts_dm <- function(constructor = c("dm", "next", "list"), ...) {
  .cstr_options("dm", constructor = constructor[[1]], ...)
}

#' @export
#' @method .cstr_construct dm
.cstr_construct.dm <- function(x, ...) {
  opts <- list(...)$opts$dm %||% opts_dm()
  if (is_corrupted_dm(x) || opts$constructor == "next") return(NextMethod())
  UseMethod(".cstr_construct.dm", structure(NA, class = opts$constructor))
}

is_corrupted_dm <- function(x) {
  # TODO
  FALSE
}

#' @export
#' @method .cstr_construct.dm dm
.cstr_construct.dm.dm <- function(x, ...) {
  def <- unclass(x)$def
  named_list_of_tables <- set_names(def$data, def$table)
  code <- .cstr_apply(
    named_list_of_tables,
    fun = "dm::dm",
    trailing_comma = TRUE,
    implicit_names = TRUE,
    ...
    )


  pk_code <- unlist(Map(
    function(table, pk_tibble) {
      if (!nrow(pk_tibble)) return(character())
      column_code <- .cstr_construct(pk_tibble$column[[1]], ...)
      paste0("dm::dm_add_pk(", protect(table), ", ", paste(column_code, collapse = "\n"), ")")
    } ,
    def$table,
    def$pks,
    USE.NAMES = FALSE))
  if (length(pk_code)) {
    # FIXME: not compatible with one liners
    pipe_collapse <- paste0(" ", get_pipe_symbol(list(...)$pipe), "\n  ")
    pk_code <- paste(pk_code, collapse = pipe_collapse)
    code <- .cstr_pipe(code, pk_code, ...)
  }

  fk_code <- unlist(Map(
    function(ref_table, fk_tibble) {
      if (!nrow(fk_tibble)) return(character())
      Map(function(table, column, ref_column) {
        paste0(
          "dm::dm_add_fk(",
          protect(table), ", ",
          paste(.cstr_construct(column, ...), collapse = "\n"), ", ",
          protect(ref_table), ", ",
          paste(.cstr_construct(ref_column, ...), collapse = "\n"), ")"
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
    fk_code <- paste(fk_code, collapse = pipe_collapse)
    code <- .cstr_pipe(code, fk_code, ...)
  }

  colors <- set_names(def$table, def$display)[!is.na(def$display)]
  if (length(colors)) {
    color_code <- .cstr_apply(colors, "dm::dm_set_colors", ...)
    code <- .cstr_pipe(code, color_code, ...)
  }
  code <- split_by_line(code)
  repair_attributes_dm(x, code, ...)
}

#' @export
#' @method .cstr_construct.dm list
.cstr_construct.dm.list <- function(x, ...) {
  .cstr_construct.list(x, ...)
}

repair_attributes_dm <- function(x, code, ...) {
  .cstr_repair_attributes(
    x, code, ...,
    idiomatic_class = "dm",
    ignore = "version"
  )
}
