# Constructive options for class 'clock_iso_year_week_day'

These options will be used on objects of class
'clock_iso_year_week_day'.

## Usage

``` r
opts_clock_iso_year_week_day(
  constructor = c("iso_year_week_day", "next", "list"),
  ...
)
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
\<constructive_options/constructive_options_clock_iso_year_week_day\>

## Details

Depending on `constructor`, we construct the object as follows:

- `"iso_year_week_day"` (default): We build the object using
  [`clock::iso_year_week_day()`](https://clock.r-lib.org/reference/iso_year_week_day.html),
  providing the components required by the precision.

- `"next"` : Use the constructor for the next supported class.

- `"list"` : We define as a list and repair attributes.
