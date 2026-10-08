# Constructive options for class 'clock_weekday'

These options will be used on objects of class 'clock_weekday'.

## Usage

``` r
opts_clock_weekday(constructor = c("weekday", "next", "integer"), ...)
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
\<constructive_options/constructive_options_clock_weekday\>

## Details

Depending on `constructor`, we construct the object as follows:

- `"weekday"` (default): We build the object using
  [`clock::weekday()`](https://clock.r-lib.org/reference/weekday.html)
  on day codes, from 1 (Sunday) to 7 (Saturday).

- `"next"` : Use the constructor for the next supported class.

- `"integer"` : We define as an atomic vector and repair attributes.
