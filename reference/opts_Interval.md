# Constructive options for class 'Interval'

These options will be used on objects of class 'Interval'.

## Usage

``` r
opts_Interval(constructor = c("interval", "next"), ...)
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
\<constructive_options/constructive_options_Interval\>

## Details

Depending on `constructor`, we construct the object as follows:

- `"interval"` (default): We build the object using
  [`lubridate::interval()`](https://lubridate.tidyverse.org/reference/interval.html)
  on "POSIXct" `start` and `end` vectors. If the end dates can't be
  computed in a way that reproduces the object exactly (floating point
  issues), or if the time zone of the start dates differs from the
  `tzone` slot, we fall back to the next constructor.

- `"next"` : Use the constructor for the next supported class.
