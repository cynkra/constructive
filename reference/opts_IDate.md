# Constructive options for class 'IDate'

These options will be used on objects of class 'IDate'.

## Usage

``` r
opts_IDate(constructor = c("as.IDate", "next", "integer"), ...)
```

## Arguments

- constructor:

  String. Name of the function used to construct the object, see Details
  section.

- ...:

  Additional options used by user defined constructors through the
  `opts` object

## Value

An object of class \<constructive_options/constructive_options_IDate\>

## Details

Depending on `constructor`, we construct the object as follows:

- `"as.IDate"` (default): We build the object using
  [`data.table::as.IDate()`](https://rdrr.io/pkg/data.table/man/IDateTime.html)
  on a character vector, or on an integer vector if some dates cannot be
  formatted as `"YYYY-MM-DD"` strings (years outside of 1 to 9999).

- `"next"` : Use the constructor for the next supported class. Call
  [`.class2()`](https://rdrr.io/r/base/class.html) on the object to see
  in which order the methods will be tried.

- `"integer"` : We define as an integer vector and repair attributes.
