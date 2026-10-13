# Constructive options for class 'ecdf'

These options will be used on objects of class 'ecdf'.

## Usage

``` r
opts_ecdf(constructor = c("ecdf", "next"), ...)
```

## Arguments

- constructor:

  String. Name of the function used to construct the object, see Details
  section.

- ...:

  Additional options used by user defined constructors through the
  `opts` object

## Value

An object of class \<constructive_options/constructive_options_ecdf\>

## Details

Depending on `constructor`, we construct the object as follows:

- `"ecdf"` (default): We build the object using
  [`ecdf()`](https://rdrr.io/r/stats/ecdf.html), the sorted non-missing
  data is retrieved from the function's enclosure. The "call" attribute
  is repaired if it differs from the constructed call.

- `"next"` : Use the constructor for the next supported class. Call
  [`.class2()`](https://rdrr.io/r/base/class.html) on the object to see
  in which order the methods will be tried. This will usually be
  equivalent to
  [`opts_stepfun()`](https://cynkra.github.io/constructive/reference/opts_stepfun.md).
