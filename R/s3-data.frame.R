#' Constructive options for class 'data.frame'
#'
#' These options will be used on objects of class 'data.frame'.
#'
#' Depending on `constructor`, we construct the object as follows:
#' * `"data.frame"` (default): Wrap the column definitions in a `data.frame()` call. If some
#'   columns are lists or data frames, we wrap the column definitions in `tibble::tibble()`.
#'   then use `as.data.frame()`.
#' * `"read.table"` : We build the object using `read.table()` if possible, or fall
#'   back to `data.frame()`.
#' * `"next"` : Use the constructor for the next supported class. Call `.class2()`
#'   on the object to see in which order the methods will be tried.
#' * `"list"` : Use `list()` and treat the class as a regular attribute.
#'
#' @param constructor String. Name of the function used to construct the object, see Details section.
#' @param recycle Boolean. For the `"data.frame"` constructor. Whether to recycle
#'   scalars to compress the output.
#' @inheritParams opts_atomic
#' @return An object of class <constructive_options/constructive_options_data.frame>
#' @export
opts_data.frame <- function(constructor = c("data.frame", "read.table", "next", "list"), ..., recycle = TRUE) {
  abort_not_boolean(recycle)
  .cstr_options("data.frame", constructor = constructor[[1]], ..., recycle = recycle)
}

#' @export
#' @method .cstr_construct data.frame
.cstr_construct.data.frame <- function(x, ...) {
  opts <- list(...)$opts$data.frame %||% opts_data.frame()
  if (is_corrupted_data.frame(x) || opts$constructor == "next") return(NextMethod())
  UseMethod(".cstr_construct.data.frame", structure(NA, class = opts$constructor))
}

is_corrupted_data.frame <- function(x) {
  if (!is.list(x) || any(sapply(unclass(x), is.null))) return(TRUE)
  attrs <- attributes(x)
  if (!all(c("names", "class", "row.names") %in% names(attrs))) return(TRUE)
  if (!is.character(attrs$names) || length(attrs$names) != length(x)) return(TRUE)
  elements_and_row_names_all_have_same_length <-
    length(unique(vapply(c(list(attrs$row.names), x), NROW, integer(1)))) == 1
  if (!elements_and_row_names_all_have_same_length) return(TRUE)

  # this might not really be corruption but data.frame() and read.table()
  # can't create columns that don't have a as.data.frame method
  # so we fall back on the next class constructor for those
  methods_ <- gsub("^as.data.frame.(.*)?\\*?$", "\\1", methods("as.data.frame"))
  has_method <- function(x) {
    any(class(x) %in% methods_)
  }
  if (!all(sapply(x, has_method))) return(TRUE)

  FALSE
}

#' @export
#' @method .cstr_construct.data.frame list
.cstr_construct.data.frame.list <- function(x, ...) {
  .cstr_construct.list(x, ...)
}

#' @export
#' @method .cstr_construct.data.frame read.table
.cstr_construct.data.frame.read.table <- function(x, ...) {
  # Fall back on data.frame constructor if relevant
  if (!nrow(x)) {
    return(.cstr_construct.data.frame.data.frame(x, ...))
  }

  rn <- attr(x, "row.names")
  numeric_row_names_are_not_default <- is.numeric(rn) && !identical(rn, seq_len(nrow(x)))
  if (numeric_row_names_are_not_default) {
    return(.cstr_construct.data.frame.data.frame(x, ...))
  }

  some_cols_are_not_atomic_vectors <-
    any(!vapply(x, function(x) is.atomic(x) && is.vector(x), logical(1)))
  if (some_cols_are_not_atomic_vectors) {
    return(.cstr_construct.data.frame.data.frame(x, ...))
  }

  some_cols_are_problematic_char <-
    any(vapply(x, FUN.VALUE = logical(1), FUN = function(x) {
      is.character(x) &&
        !any(is.na(suppressWarnings(as.numeric(x)))) &&
        !grepl("[\"']", x)
    }))
  if (some_cols_are_problematic_char) {
    return(.cstr_construct.data.frame.data.frame(x, ...))
  }

  # fill a data frame with deparsed values
  code_df <- x
  code_df[] <- lapply(x, function(x) {
    if (is.character(x)) sprintf("'%s'", x) else sapply(x, function(x, ...) .cstr_construct(x, ...), ...)
  })
  dbl_cols <- sapply(x, is.double)

  # make sure double values will be read as double by adding a dot at the end of integerish values
  # and align them
  code_df[dbl_cols] <- lapply(code_df[dbl_cols], function(col) align_numerics(sub("^(\\d+)$", "\\1.", col)))

  # include headers and row names in the table
  code_df <- rbind(names(x), code_df)
  rn <- rownames(x)
  if (is.character(attr(x, "row.names"))) {
    code_df <- cbind(c("", sprintf("'%s'", rownames(x))), code_df)
  }
  code_df[] <- lapply(code_df, format, justify = "right")

  # collapse table into code
  code <- paste(
    c("read.table(header = TRUE, text = \"", do.call(paste, code_df), "\")"),
    collapse = if (list(...)$one_liner) "\\n" else "\n"
  )

  # repair
  repair_attributes_data.frame(x, code, ...)
}

align_numerics <- function(x) {
  dot_pos <- unlist(gregexpr(".", x, fixed = TRUE))
  dot_pos[dot_pos == -1] <- NA
  digits <- nchar(x) - dot_pos
  digits[is.na(digits)] <- 0
  paste0(x, strrep(" ", max(digits) - digits))
}

#' @export
#' @method .cstr_construct.data.frame data.frame
.cstr_construct.data.frame.data.frame <- function(x, ...) {
  opts <- list(...)$opts$data.frame %||% opts_data.frame()
  # Fall back on list constructor if relevant
  df_has_list_cols <- any(sapply(x, function(col) is.list(col) && !inherits(col, "AsIs")))
  if (df_has_list_cols) return(.cstr_construct.list(x, ...))
  arg_names <- c("row.names", "check.rows", "check.names", "fix.empty.names", "stringsAsFactors")
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

  # include row.names arg only if necessary
  rn <- attr(x, "row.names")

  # The automatic row names are defined oddly, from doc:
  # If row names are not supplied in the call to data.frame, the row names are
  # taken from the first component that has suitable names
  if (!ncol(x) || !identical(rn, seq_len(nrow(x)))) args <- c(args, list(row.names = rn))

  # include check.names arg only if necessary
  if (!any(names(x) %in% c("", NA)) && (any(!is_syntactic(names(x))) || anyDuplicated(names(x)))) {
    args <- c(args, list(check.names = FALSE))
  }

  # build code recursively
  code <- .cstr_apply(args, fun = "data.frame", ...)

  # repair
  repair_attributes_data.frame(x, code, ..., repair_names = repair_names)
}

repair_attributes_data.frame <- function(x, code, ..., pipe = NULL) {
  ignore <- "row.names"
  if (identical(names(x), character())) ignore <- c(ignore, "names")
  .cstr_repair_attributes(
    x, code, ...,
    pipe = pipe,
    ignore = ignore,
    idiomatic_class = "data.frame"
  )
}
