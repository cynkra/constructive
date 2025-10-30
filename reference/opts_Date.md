# Constructive options class 'Date'

These options will be used on objects of class 'date'.

## Usage

``` r
opts_Date(
  constructor = c("as.Date", "as_date", "date", "new_date", "as.Date.numeric",
    "as_date.numeric", "next", "double"),
  ...,
  origin = "1970-01-01"
)
```

## Arguments

- constructor:

  String. Name of the function used to construct the object.

- ...:

  Additional options used by user defined constructors through the
  `opts` object

- origin:

  Origin to be used, ignored when irrelevant.

## Value

An object of class \<constructive_options/constructive_options_Date\>

## Details

Depending on `constructor`, we construct the object as follows:

- `"as.Date"` (default): We wrap a character vector with
  [`as.Date()`](https://rdrr.io/r/base/as.Date.html), if the date is
  infinite it cannot be converted to character and we wrap a numeric
  vector and provide an `origin` argument.

- `"as_date"` : Similar as above but using
  [`lubridate::as_date()`](https://lubridate.tidyverse.org/reference/as_date.html),
  the only difference is that we never need to supply `origin`.

- `"date"` : Similar as above but using
  [`lubridate::date()`](https://lubridate.tidyverse.org/reference/date.html),
  it doesn't support infinite dates so we fall back on
  [`lubridate::as_date()`](https://lubridate.tidyverse.org/reference/as_date.html)
  when we encounter them.

- `"new_date"` : We wrap a numeric vector with
  [`vctrs::new_date()`](https://vctrs.r-lib.org/reference/new_date.html)

- `"as.Date.numeric"` : We wrap a numeric vector with
  [`as.Date()`](https://rdrr.io/r/base/as.Date.html) and use the
  provided `origin`

- `"as_date.numeric"` : Same as above but using
  [`lubridate::as_date()`](https://lubridate.tidyverse.org/reference/as_date.html)
  and use the provided `origin`

- `"next"` : Use the constructor for the next supported class. Call
  [`.class2()`](https://rdrr.io/r/base/class.html) on the object to see
  in which order the methods will be tried.

- `"double"` : We define as an double vector and repair attributes

If the data is not appropriate for a constructor we fall back to another
one appropriately.
