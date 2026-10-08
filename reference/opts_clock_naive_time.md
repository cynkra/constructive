# Constructive options for class 'clock_naive_time'

These options will be used on objects of class 'clock_naive_time'.

## Usage

``` r
opts_clock_naive_time(constructor = c("as_naive_time", "next", "list"), ...)
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
\<constructive_options/constructive_options_clock_naive_time\>

## Details

Depending on `constructor`, we construct the object as follows:

- `"as_naive_time"` (default): We build the object using
  [`clock::as_naive_time()`](https://clock.r-lib.org/reference/as_naive_time.html)
  on a
  [`clock::year_month_day()`](https://clock.r-lib.org/reference/year_month_day.html)
  call of the same precision.

- `"next"` : Use the constructor for the next supported class.

- `"list"` : We define as a list and repair attributes.
