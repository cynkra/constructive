# Constructive options for class 'clock_year_month_weekday'

These options will be used on objects of class
'clock_year_month_weekday'.

## Usage

``` r
opts_clock_year_month_weekday(
  constructor = c("year_month_weekday", "next", "list"),
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
\<constructive_options/constructive_options_clock_year_month_weekday\>

## Details

Depending on `constructor`, we construct the object as follows:

- `"year_month_weekday"` (default): We build the object using
  [`clock::year_month_weekday()`](https://clock.r-lib.org/reference/year_month_weekday.html),
  providing the components required by the precision.

- `"next"` : Use the constructor for the next supported class.

- `"list"` : We define as a list and repair attributes.
