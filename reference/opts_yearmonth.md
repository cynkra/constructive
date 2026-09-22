# Constructive options for class 'yearmonth'

These options will be used on objects of class 'yearmonth'.

## Usage

``` r
opts_yearmonth(constructor = c("yearmonth", "next"), ...)
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
\<constructive_options/constructive_options_yearmonth\>

## Details

Depending on `constructor`, we construct the object as follows:

- `"yearmonth"` (default): We build the object using
  [`tsibble::yearmonth()`](https://tsibble.tidyverts.org/reference/year-month.html)
  on a string in the format `"2024 Jan"`. If the object contains `NA`s
  or years that can't be parsed from a string, we use a `Date` object as
  an input instead.

- `"next"` : Use the constructor for the next supported class. Call
  [`.class2()`](https://rdrr.io/r/base/class.html) on the object to see
  in which order the methods will be tried.
