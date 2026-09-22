# Constructive options for class 'sfc'

These options will be used on objects of class 'sfc', simple feature
geometry list-columns from the 'sf' package.

## Usage

``` r
opts_sfc(constructor = c("st_sfc", "next"), ...)
```

## Arguments

- constructor:

  String. Name of the function used to construct the object, see Details
  section.

- ...:

  Additional options used by user defined constructors through the
  `opts` object

## Value

An object of class \<constructive_options/constructive_options_sfc\>

## Details

Depending on `constructor`, we construct the object as follows:

- `"st_sfc"` (default): We build the object using
  [`sf::st_sfc()`](https://r-spatial.github.io/sf/reference/sfc.html) on
  the geometries, with `crs` and `precision` arguments if relevant. The
  other attributes ("bbox", "n_empty", "classes"...) are computed by
  [`sf::st_sfc()`](https://r-spatial.github.io/sf/reference/sfc.html)
  and are repaired only if they don't match.

- `"next"` : Use the constructor for the next supported class. Call
  [`.class2()`](https://rdrr.io/r/base/class.html) on the object to see
  in which order the methods will be tried.
