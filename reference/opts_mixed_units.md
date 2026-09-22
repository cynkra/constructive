# Constructive options for class 'mixed_units'

These options will be used on objects of class 'mixed_units'.

## Usage

``` r
opts_mixed_units(constructor = c("mixed_units", "next"), ...)
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
\<constructive_options/constructive_options_mixed_units\>

## Details

Depending on `constructor`, we construct the object as follows:

- `"mixed_units"` (default): We build the object using
  [`units::mixed_units()`](https://r-quantities.github.io/units/reference/mixed_units.html)
  on a double vector and a character vector of unit strings as given by
  [`units::deparse_unit()`](https://r-quantities.github.io/units/reference/deparse_unit.html).

- `"next"` : Use the constructor for the next supported class. Call
  [`.class2()`](https://rdrr.io/r/base/class.html) on the object to see
  in which order the methods will be tried.
