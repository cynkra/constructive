#' @export
construct_idiomatic.environment <- function(x, ...) {
  # this is not very robust but might help in some useful special cases

  # The name of `asNamespace("pkg")` is always "pkg" and print as `<environment: namespace:pkg>`
  # The name of `as.environment("package:pkg")` is ALMOST always "package:pkg" and prints as
  #  `<environment: package:pkg>` + attributes
  # The exception is `as.environment("package:base")` which prints as
  #   `<environment: base>` and whose name is "base"
  # This means `asNamespace("base")` (a.k.a. `.BaseNamespaceEnv`) and
  #   `as.environment("package:base")` (a.k.a. `baseenv()`) have the same name
  #   but are different. So we implement a workaround.
  if(identical(x, baseenv())) return('baseenv()')
  name <- environmentName(x)
  if (name == "R_GlobalEnv") return(".GlobalEnv")
  if (name == "base") return(".BaseNamespaceEnv")
  if (name %in% row.names(installed.packages())) return(sprintf('asNamespace("%s")', name))
  if (name %in% search()) return(sprintf('as.environment("%s")', name))
  wrap(construct_apply(as.list(x)), "as.environment", new_line = FALSE)
}

#' @export
repair_attributes.environment <- function(x, code, pipe ="base", ...) {
  repair_attributes_impl(
    x, code, pipe,
    ignore = c("name", "path"),
    ...
  )
}

#' @export
construct_idiomatic.function <- function(x, pipe, ...) {
  x_lst <- as.list(x)
  # rlang::expr_deparse changes the body by putting parentheses around f <- (function(){})
  # so we must use regular deparse
  fun_lst <- lapply(x_lst, deparse)
  args0 <- head(fun_lst, -1)
  body0 <- fun_lst[[length(fun_lst)]]
  # a srcref is created if the body starts with `{`
  srcrefed <- startsWith(body0[[1]], "{")
  args <- construct_apply(args0, "alist", language = TRUE, ...)
  body <- construct_apply(list(body0), "quote", language = TRUE, ...)
  env <- construct_raw(environment(x))
  code <- construct_apply(list(args, body, env), "rlang::new_function", language = TRUE)
  if (srcrefed) pipe(code, "rlang::zap_srcref()", pipe) else code
}

#' @export
repair_attributes.function <- function(x, code, pipe ="base", ...) {
  repair_attributes_impl(
    x, code, pipe,
    ignore = c("name", "path"),
    ...
  )
}

#' @export
construct_idiomatic.dm <- function(x, pipe = "base", ...) {
  def <- unclass(x)$def
  named_list_of_tables <- set_names(def$data, def$table)
  code <- construct_apply(
    named_list_of_tables, fun = "dm::dm", keep_trailing_comma = TRUE, implicit_names = TRUE, ...)

  pk_code <- unlist(Map(
    function(table, pk_tibble) {
      if(!nrow(pk_tibble)) return(character())
      column_code <- construct_raw(pk_tibble$column[[1]], ...)
      paste0("dm::dm_add_pk(", protect(table), ", ", paste(column_code, collapse = "\n") , ")")
  } ,
  def$table,
  def$pks,
  USE.NAMES = FALSE))
  if(length(pk_code)) {
    pk_code <- paste(pk_code, collapse = "|>\n")
    code <- pipe(code, pk_code, pipe)
  }

  fk_code <- unlist(Map(
    function(ref_table, fk_tibble) {
      if(!nrow(fk_tibble)) return(character())
      Map(function(table, column, ref_column) {
        paste0(
          "dm::dm_add_fk(",
          protect(table), ", ",
          paste(construct_raw(column, ...), collapse = "\n"), ",",
          protect(ref_table), ", ",
          paste(construct_raw(ref_column, ...), collapse = "\n"), ")"
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
    code <- pipe(code, fk_code, pipe)
  }

  colors <- set_names(def$table, def$display)[!is.na(def$display)]
  if (length(colors)) {
    color_code <- construct_apply(colors, "dm::dm_set_colors", ...)
    code <- pipe(code, color_code, pipe)
  }

  code
}


#' @export
repair_attributes.dm <- function(x, code, pipe ="base", ...) {
  repair_attributes_impl(
    x, code, pipe,
    idiomatic_class = "dm",
    ignore = c("version"),
    ...
  )
}
