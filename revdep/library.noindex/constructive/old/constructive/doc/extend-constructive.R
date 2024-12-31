## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup--------------------------------------------------------------------
library(constructive)

## -----------------------------------------------------------------------------
.cstr_construct
# a character vector
.cstr_construct(letters)
# a constructive object, 
construct(letters)

## -----------------------------------------------------------------------------
.cstr_construct.Date <- function(x, ...) {
  opts <- list(...)$opts$Date %||% opts_Date()
  if (is_corrupted_Date(x) || opts$constructor == "next") return(NextMethod())
  UseMethod(".cstr_construct.Date", structure(NA, class = opts$constructor))
}

## ----eval=FALSE---------------------------------------------------------------
#  opts_Date <- function(
#      constructor = c(
#        "as.Date", "as_date", "date", "new_date", "as.Date.numeric", "as_date.numeric", "next", "double"),
#      ...,
#      origin = "1970-01-01") {
#    .cstr_options("Date", constructor = constructor[[1]], ..., origin = origin)
#  }

## ----error = TRUE-------------------------------------------------------------
x <- structure("12345", class = "Date")
x
x + 1

## -----------------------------------------------------------------------------
is_corrupted_Date <- function(x) {
  !is.double(x)
}

## -----------------------------------------------------------------------------
construct(x)

## -----------------------------------------------------------------------------
constructive:::.cstr_construct.Date.as.Date

## -----------------------------------------------------------------------------
x <- structure(c(12345, 20000), class = "Date")
y <- structure(c(12345, Inf), class = "Date")
construct(x)
construct(y)

## -----------------------------------------------------------------------------
x <- structure(c(12345, 20000), class = "Date", some_attr = 42)
# attributes are not visible due to "Date"'s printing method
x

construct(x)

## -----------------------------------------------------------------------------
constructive:::repair_attributes_Date

constructive:::repair_attributes_factor

constructive:::repair_attributes_tbl_df

