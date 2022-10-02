# no need for a constructor for grouped_df since it falls back on tbl_df
#' @export
repair_attributes.grouped_df <- function(x, code, pipe = "base", one_liner = FALSE, ...) {
  grps <- head(names(attr(x, "groups")), -1)
  group_by_code <- construct_apply(
    grps,
    "dplyr::group_by",
    language = TRUE,
    pipe = pipe,
    one_liner = one_liner,
    ...
  )
  code <- pipe(
    code,
    group_by_code,
    pipe = pipe,
    one_liner = one_liner
  )
  repair_attributes_impl(
    x, code, pipe,
    ignore = c("row.names", "groups"),
    idiomatic_class = c("grouped_df", "tbl_df", "tbl", "data.frame"),
    one_liner = one_liner,
    ...
  )
}
