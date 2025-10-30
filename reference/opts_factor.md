# Constructive options for class 'factor'

These options will be used on objects of class 'factor'.

## Usage

``` r
opts_factor(
  constructor = c("factor", "as_factor", "new_factor", "next", "integer"),
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

An object of class \<constructive_options/constructive_options_factor\>

## Details

Depending on `constructor`, we construct the object as follows:

- `"factor"` (default): Build the object using
  [`factor()`](https://rdrr.io/r/base/factor.html), levels won't be
  defined explicitly if they are in alphabetical order (locale
  dependent!)

- `"as_factor"` : Build the object using
  [`forcats::as_factor()`](https://forcats.tidyverse.org/reference/as_factor.html)
  whenever possible, i.e. when levels are defined in order of appearance
  in the vector. Otherwise falls back to `"factor"` constructor.

- `"new_factor"` : Build the object using
  [`vctrs::new_factor()`](https://vctrs.r-lib.org/reference/new_factor.html).
  Levels are always defined explicitly.

- `"next"` : Use the constructor for the next supported class. Call
  [`.class2()`](https://rdrr.io/r/base/class.html) on the object to see
  in which order the methods will be tried.

- `"integer"` : We define as an integer vector and repair attributes.
