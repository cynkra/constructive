# no need for a constructor for grouped_df since it falls back on tbl_df
#' @export
repair_attributes.rowwise_df <- function(x, code, ..., pipe = "base", one_liner = FALSE) {
  opts <- fetch_opts("tbl_df", ...)
  if (opts$constructor == "list") {
    return(repair_attributes.default(x, code, ..., pipe = pipe))
  }
  vars <- head(names(attr(x, "groups")), -1)
  rowwise_code <- construct_apply(
    vars,
    "dplyr::rowwise",
    ...,
    language = TRUE,
    pipe = pipe,
    one_liner = one_liner
  )
  code <- pipe(
    code,
    rowwise_code,
    pipe = pipe,
    one_liner = one_liner
  )
  repair_attributes_impl(
    x, code, ...,
    pipe = pipe,
    ignore = c("row.names", "groups"),
    idiomatic_class = c("rowwise_df", "tbl_df", "tbl", "data.frame"),
    one_liner = one_liner
  )
}

