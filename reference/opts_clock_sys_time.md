# Constructive options for class 'clock_sys_time'

These options will be used on objects of class 'clock_sys_time'.

## Usage

``` r
opts_clock_sys_time(constructor = c("as_sys_time", "next", "list"), ...)
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
\<constructive_options/constructive_options_clock_sys_time\>

## Details

Depending on `constructor`, we construct the object as follows:

- `"as_sys_time"` (default): We build the object using
  [`clock::as_sys_time()`](https://clock.r-lib.org/reference/as_sys_time.html)
  on a
  [`clock::year_month_day()`](https://clock.r-lib.org/reference/year_month_day.html)
  call of the same precision.

- `"next"` : Use the constructor for the next supported class.

- `"list"` : We define as a list and repair attributes.
