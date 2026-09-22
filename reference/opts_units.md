# Constructive options for class 'units'

These options will be used on objects of class 'units'.

## Usage

``` r
opts_units(constructor = c("set_units", "as_units", "next"), ...)
```

## Arguments

- constructor:

  String. Name of the function used to construct the object, see Details
  section.

- ...:

  Additional options used by user defined constructors through the
  `opts` object

## Value

An object of class \<constructive_options/constructive_options_units\>

## Details

Depending on `constructor`, we construct the object as follows:

- `"set_units"` (default): We build the object using
  `units::set_units(x, value, mode = "standard")` where `value` is the
  unit string as given by
  [`units::deparse_unit()`](https://r-quantities.github.io/units/reference/deparse_unit.html).

- `"as_units"` : We build the object using `units::as_units(x, value)`.

- `"next"` : Use the constructor for the next supported class. Call
  [`.class2()`](https://rdrr.io/r/base/class.html) on the object to see
  in which order the methods will be tried.
