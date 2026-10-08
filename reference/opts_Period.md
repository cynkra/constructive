# Constructive options for class 'Period'

These options will be used on objects of class 'Period'.

## Usage

``` r
opts_Period(constructor = c("period", "next"), ...)
```

## Arguments

- constructor:

  String. Name of the function used to construct the object, see Details
  section.

- ...:

  Additional options used by user defined constructors through the
  `opts` object

## Value

An object of class \<constructive_options/constructive_options_Period\>

## Details

Depending on `constructor`, we construct the object as follows:

- `"period"` (default): We build the object using
  [`lubridate::period()`](https://lubridate.tidyverse.org/reference/period.html),
  providing the `years`, `months`, `days`, `hours`, `minutes` and
  `seconds` arguments, omitting the components that are zero for all
  elements.

- `"next"` : Use the constructor for the next supported class.
