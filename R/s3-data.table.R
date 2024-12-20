#' Constructive options for class 'data.table'
#'
#' These options will be used on objects of class 'data.table'.
#'
#' Depending on `constructor`, we construct the object as follows:
#' * `"data.table"` (default): Wrap the column definitions in a `data.table()` call.
#' * `"next"` : Use the constructor for the next supported class. Call `.class2()`
#'   on the object to see in which order the methods will be tried.
#' * `"list"` : Use `list()` and treat the class as a regular attribute.
#'
#' @param constructor String. Name of the function used to construct the object, see Details section.
#' @param selfref Boolean. Whether to include the `.internal.selfref` attribute. It's
#'   probably not useful, hence the default, `waldo::compare()` is used to assess the output
#'   fidelity and doesn't check it, but if you really need to generate code that builds
#'   an object `identical()` to the input you'll need to set this to `TRUE`.#'
#' @param recycle Boolean. Whether to recycle scalars to compress the output.
#' @inheritParams opts_atomic
#' @return An object of class <constructive_options/constructive_options_data.table>
#' @export
opts_data.table <- function(constructor = c("data.table", "next", "list"), ..., selfref = FALSE, recycle = TRUE) {
  abort_not_boolean(recycle)
  .cstr_options("data.table", constructor = constructor[[1]], ..., selfref = selfref, recycle = recycle)
}

#' @export
#' @method .cstr_construct data.table
.cstr_construct.data.table <- function(x, ...) {
  opts <- list(...)$opts$data.table %||% opts_data.table()
  if (is_corrupted_data.table(x) || opts$constructor == "next") return(NextMethod())
  UseMethod(".cstr_construct.data.table", structure(NA, class = opts$constructor))
}

is_corrupted_data.table <- function(x) {
  is_corrupted_data.frame(x)
}

#' @export
#' @method .cstr_construct.data.table list
.cstr_construct.data.table.list <- function(x, ...) {
  .cstr_construct.list(x, ...)
}

#' @export
#' @method .cstr_construct.data.table data.table
.cstr_construct.data.table.data.table <- function(x, ...) {
  opts <- list(...)$opts$data.table %||% opts_data.table()
  # Fall back on list constructor if relevant
  arg_names <- c("keep.rownames", "check.names", "key", "stringsAsFactors")
  problematic_names_lgl <- names(x) %in% c(arg_names, "", NA)
  repair_names <- any(problematic_names_lgl)
  args <- x
  if (repair_names) names(args)[problematic_names_lgl] <- ""
  # recycle value for constant columns
  if (opts$recycle && nrow(x) > 1 && ncol(x) > 1) {
    # recycling depends on S3 subsetting so we can't be general here, but we might
    # extend this list
    # note : "POSIXlt" is coerced to "POSIXct" in data.frame so not relevant here
    recyclable_classes <-
      list(NULL, "factor", c("ordered", "factor"), "Date", c("POSIXct", "POSIXt"))
    args <- lapply(args, function(x) {
      if (
        any(sapply(recyclable_classes, identical, oldClass(x))) &&
        length(unique(x)) == 1 # &&
        # !anyDuplicated(names(x)) # not necessary for data frames, but yes for tibble
      ) {
        return(base::`[`(x, 1))
      }
      x
    })
    if (all(lengths(args) == 1)) args[1] <- x[1]
  }

  key <- attr(x, "sorted")
  if (!is.null(key)) {
    args <- c(args, key = key)
  }

  code <- .cstr_apply(args, fun = "data.table::data.table", ...)
  repair_attributes_data.table(x, code, ..., selfref = opts$selfref, repair_names = repair_names)
}

repair_attributes_data.table <- function(x, code, ..., pipe = NULL, selfref = FALSE) {
  ignore <- c("row.names", "sorted")
  if (!selfref) ignore <- c(ignore, ".internal.selfref")
  if (identical(names(x), character())) ignore <- c(ignore, "names")
  .cstr_repair_attributes(
    x, code, ...,
    pipe = pipe,
    ignore = ignore,
    idiomatic_class = c("data.table", "data.frame")
  )
}
