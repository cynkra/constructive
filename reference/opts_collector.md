# Constructive options for class 'collector'

These options will be used on objects of class 'collector', i.e. column
specifications as created by
[`readr::col_double()`](https://readr.tidyverse.org/reference/parse_atomic.html),
[`readr::col_date()`](https://readr.tidyverse.org/reference/parse_datetime.html),
[`readr::col_factor()`](https://readr.tidyverse.org/reference/parse_factor.html)
etc. They all share the class 'collector' and a subclass 'collector\_'
that maps to the constructor `readr::col_<type>()`.

## Usage

``` r
opts_collector(constructor = c("col", "next", "list"), ...)
```

## Arguments

- constructor:

  String. Name of the function used to construct the object.

- ...:

  Additional options used by user defined constructors through the
  `opts` object

## Value

An object of class
\<constructive_options/constructive_options_collector\>

## Details

Depending on `constructor`, we construct the object as follows:

- `"col"` (default): We build the object using the relevant
  `readr::col_*()` function, e.g.
  [`readr::col_double()`](https://readr.tidyverse.org/reference/parse_atomic.html)
  or `readr::col_date(format = "%Y")`, providing only non default
  arguments.

- `"next"` : Use the constructor for the next supported class. Call
  [`.class2()`](https://rdrr.io/r/base/class.html) on the object to see
  in which order the methods will be tried. This will usually be
  equivalent to `"list"`

- `"list"` : We define as a list and repair attributes
