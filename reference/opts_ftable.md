# Constructive options for class 'ftable'

These options will be used on objects of class 'ftable'.

## Usage

``` r
opts_ftable(constructor = c("ftable", "next"), ...)
```

## Arguments

- constructor:

  String. Name of the function used to construct the object, see Details
  section.

- ...:

  Additional options used by user defined constructors through the
  `opts` object

## Value

An object of class \<constructive_options/constructive_options_ftable\>

## Details

Depending on `constructor`, we construct the object as follows:

- `"ftable"` (default): We build the object using
  [`ftable()`](https://rdrr.io/r/stats/ftable.html) on a table,
  providing the `row.vars` argument when needed.

- `"next"` : Use the constructor for the next supported class. Call
  [`.class2()`](https://rdrr.io/r/base/class.html) on the object to see
  in which order the methods will be tried. This will usually be
  equivalent to `opts_matrix("matrix")`.
