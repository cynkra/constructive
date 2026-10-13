# Constructive options for class 'ITime'

These options will be used on objects of class 'ITime'.

## Usage

``` r
opts_ITime(constructor = c("as.ITime", "next", "integer"), ...)
```

## Arguments

- constructor:

  String. Name of the function used to construct the object, see Details
  section.

- ...:

  Additional options used by user defined constructors through the
  `opts` object

## Value

An object of class \<constructive_options/constructive_options_ITime\>

## Details

Depending on `constructor`, we construct the object as follows:

- `"as.ITime"` (default): We build the object using
  [`data.table::as.ITime()`](https://rdrr.io/pkg/data.table/man/IDateTime.html)
  on a character vector in the format `"HH:MM:SS"`. Negative times and
  times of 24 hours or more cannot be built this way so we fall back to
  the `"integer"` constructor.

- `"next"` : Use the constructor for the next supported class. Call
  [`.class2()`](https://rdrr.io/r/base/class.html) on the object to see
  in which order the methods will be tried.

- `"integer"` : We define as an integer vector and repair attributes.
