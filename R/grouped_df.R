# no need for a constructor for grouped_df since it falls back on tbl_df
#' @export
repair_attributes.grouped_df <- function(x, code, pipe = "base", ...) {
  grps <- head(names(attr(x, "groups")), -1)
  group_by_code <- construct_apply(
    grps,
    "dplyr::group_by",
    language = TRUE,
    ...
  )
  code <- pipe(
    code,
    group_by_code,
    pipe = pipe
  )
  repair_attributes_impl(
    x, code, pipe,
    ignore = c("row.names", "groups"),
    idiomatic_class = c("grouped_df", "tbl_df", "tbl", "data.frame")
  )
}
