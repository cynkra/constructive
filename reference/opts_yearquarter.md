# Constructive options for class 'yearquarter'

These options will be used on objects of class 'yearquarter'.

## Usage

``` r
opts_yearquarter(constructor = c("yearquarter", "next"), ...)
```

## Arguments

- constructor:

  String. Name of the function used to construct the object, see Details
  section.

- ...:

  Additional options used by user defined constructors through the
  `opts` object

## Value

An object of class
\<constructive_options/constructive_options_yearquarter\>

## Details

Depending on `constructor`, we construct the object as follows:

- `"yearquarter"` (default): We build the object using
  [`tsibble::yearquarter()`](https://tsibble.tidyverts.org/reference/year-quarter.html)
  on a string in the format `"2024 Q1"`. If the object contains `NA`s or
  years that can't be parsed from a string, we use a `Date` object as an
  input instead.

- `"next"` : Use the constructor for the next supported class. Call
  [`.class2()`](https://rdrr.io/r/base/class.html) on the object to see
  in which order the methods will be tried.
