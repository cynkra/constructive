# Constructive options for class 'dist'

These options will be used on objects of class 'dist'.

## Usage

``` r
opts_dist(constructor = c("as.dist", "next"), ...)
```

## Arguments

- constructor:

  String. Name of the function used to construct the object, see Details
  section.

- ...:

  Additional options used by user defined constructors through the
  `opts` object

## Value

An object of class \<constructive_options/constructive_options_dist\>

## Details

Depending on `constructor`, we construct the object as follows:

- `"as.dist"` (default): We build the object using
  [`as.dist()`](https://rdrr.io/r/stats/dist.html) on a symmetric
  matrix. The "call" attribute, and the "method" attribute set by
  [`dist()`](https://rdrr.io/r/stats/dist.html), are then repaired.

- `"next"` : Use the constructor for the next supported class. Call
  [`.class2()`](https://rdrr.io/r/base/class.html) on the object to see
  in which order the methods will be tried. This will usually be
  equivalent to
  [`opts_atomic()`](https://cynkra.github.io/constructive/reference/opts_atomic.md).
