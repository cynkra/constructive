# Constructive options for class 'ordered'

These options will be used on objects of class 'ordered'.

## Usage

``` r
opts_ordered(
  constructor = c("ordered", "factor", "new_ordered", "next", "integer"),
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

An object of class \<constructive_options/constructive_options_ordered\>

## Details

Depending on `constructor`, we construct the object as follows:

- `"ordered"` (default): Build the object using
  [`ordered()`](https://rdrr.io/r/base/factor.html), levels won't be
  defined explicitly if they are in alphabetical order (locale
  dependent!)

- `"factor"` : Same as above but build the object using
  [`factor()`](https://rdrr.io/r/base/factor.html) and `ordered = TRUE`.

- `"new_ordered"` : Build the object using
  [`vctrs::new_ordered()`](https://vctrs.r-lib.org/reference/new_factor.html).
  Levels are always defined explicitly.

- `"next"` : Use the constructor for the next supported class. Call
  [`.class2()`](https://rdrr.io/r/base/class.html) on the object to see
  in which order the methods will be tried.

- `"integer"` : We define as an integer vector and repair attributes
