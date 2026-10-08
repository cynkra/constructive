# Constructive options for class 'clock_duration'

These options will be used on objects of class 'clock_duration'.

## Usage

``` r
opts_clock_duration(constructor = c("duration", "next", "list"), ...)
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
\<constructive_options/constructive_options_clock_duration\>

## Details

Depending on `constructor`, we construct the object as follows:

- `"duration"` (default): We build the object using the
  `clock::duration_*()` helper matching the precision, e.g.
  [`clock::duration_days()`](https://clock.r-lib.org/reference/duration-helper.html).
  Durations that don't fit in an integer are constructed with the
  `"list"` constructor.

- `"next"` : Use the constructor for the next supported class.

- `"list"` : We define as a list and repair attributes.
