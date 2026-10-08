# Constructive options for class 'clock_zoned_time'

These options will be used on objects of class 'clock_zoned_time'.

## Usage

``` r
opts_clock_zoned_time(constructor = c("as_zoned_time", "next", "list"), ...)
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
\<constructive_options/constructive_options_clock_zoned_time\>

## Details

Depending on `constructor`, we construct the object as follows:

- `"as_zoned_time"` (default): We build the object using
  [`clock::as_zoned_time()`](https://clock.r-lib.org/reference/as_zoned_time.html)
  on the local naive time, providing the `zone` and, when some local
  times are ambiguous, the `ambiguous` argument.

- `"next"` : Use the constructor for the next supported class.

- `"list"` : We define as a list and repair attributes.
