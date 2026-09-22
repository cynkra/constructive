# Constructive options for class 'hms'

These options will be used on objects of class 'hms'.

## Usage

``` r
opts_hms(constructor = c("as_hms", "hms", "new_hms", "next", "double"), ...)
```

## Arguments

- constructor:

  String. Name of the function used to construct the object, see Details
  section.

- ...:

  Additional options used by user defined constructors through the
  `opts` object

## Value

An object of class \<constructive_options/constructive_options_hms\>

## Details

Depending on `constructor`, we construct the object as follows:

- `"as_hms"` (default): We build the object using
  [`hms::as_hms()`](https://hms.tidyverse.org/reference/hms.html) on a
  character vector in the format `"12:34:56"`. If some values cannot be
  reproduced exactly this way (e.g. negative values, durations of more
  than 24 hours, or some fractional seconds) we fall back to the `"hms"`
  constructor.

- `"hms"` : We build the object using
  [`hms::hms()`](https://hms.tidyverse.org/reference/hms.html) on
  `seconds`, `minutes` and `hours` arguments. If the decomposition is
  not exact we provide only the `seconds` argument.

- `"new_hms"` : We build the object using
  [`hms::new_hms()`](https://hms.tidyverse.org/reference/hms.html) on a
  double vector.

- `"next"` : Use the constructor for the next supported class. Call
  [`.class2()`](https://rdrr.io/r/base/class.html) on the object to see
  in which order the methods will be tried.

- `"double"` : We define as an atomic vector and repair attributes.
