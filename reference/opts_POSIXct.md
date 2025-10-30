# Constructive options for class 'POSIXct'

These options will be used on objects of class 'POSIXct'.

## Usage

``` r
opts_POSIXct(
  constructor = c("as.POSIXct", ".POSIXct", "as_datetime", "as.POSIXct.numeric",
    "as_datetime.numeric", "next", "atomic"),
  ...,
  origin = "1970-01-01"
)
```

## Arguments

- constructor:

  String. Name of the function used to construct the object, see Details
  section.

- ...:

  Additional options used by user defined constructors through the
  `opts` object

- origin:

  Origin to be used, ignored when irrelevant.

## Value

An object of class \<constructive_options/constructive_options_POSIXct\>

## Details

Depending on `constructor`, we construct the object as follows:

- `"as.POSIXct"` (default): Build the object using a
  [`as.POSIXct()`](https://rdrr.io/r/base/as.POSIXlt.html) call on a
  character vector.

- `".POSIXct"` : Build the object using a
  [`.POSIXct()`](https://rdrr.io/r/base/base-internal.html) call on a
  numeric vector.

- `"as_datetime"` : Build the object using a
  [`lubridate::as_datetime()`](https://lubridate.tidyverse.org/reference/as_date.html)
  call on a character vector.

- `"next"` : Use the constructor for the next supported class. Call
  [`.class2()`](https://rdrr.io/r/base/class.html) on the object to see
  in which order the methods will be tried.

- `"atomic"` : We define as an atomic vector and repair attributes.

If the data is not appropriate for a constructor we fall back to another
one appropriately. In particular corrupted POSIXct objects such as those
defined on top of integers (or worse) are all constructed with the
`".POSIXct"` constructor.
