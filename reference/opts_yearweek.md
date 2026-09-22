# Constructive options for class 'yearweek'

These options will be used on objects of class 'yearweek'.

## Usage

``` r
opts_yearweek(constructor = c("yearweek", "next"), ...)
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
\<constructive_options/constructive_options_yearweek\>

## Details

Depending on `constructor`, we construct the object as follows:

- `"yearweek"` (default): We build the object using
  [`tsibble::yearweek()`](https://tsibble.tidyverts.org/reference/year-week.html)
  on a string in the format `"2024 W01"`. If the object contains `NA`s
  or years that can't be parsed from a string, we use a `Date` object as
  an input instead.

- `"next"` : Use the constructor for the next supported class. Call
  [`.class2()`](https://rdrr.io/r/base/class.html) on the object to see
  in which order the methods will be tried.
