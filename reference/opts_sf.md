# Constructive options for class 'sf'

These options will be used on objects of class 'sf', simple feature data
frames from the 'sf' package.

## Usage

``` r
opts_sf(constructor = c("st_sf", "next"), ...)
```

## Arguments

- constructor:

  String. Name of the function used to construct the object, see Details
  section.

- ...:

  Additional options used by user defined constructors through the
  `opts` object

## Value

An object of class \<constructive_options/constructive_options_sf\>

## Details

Depending on `constructor`, we construct the object as follows:

- `"st_sf"` (default): We build the object using
  [`sf::st_sf()`](https://r-spatial.github.io/sf/reference/sf.html) on
  the columns, or on a tibble if the object is also a tibble, with
  `sf_column_name` and `agr` arguments if relevant.

- `"next"` : Use the constructor for the next supported class. Call
  [`.class2()`](https://rdrr.io/r/base/class.html) on the object to see
  in which order the methods will be tried.

[`sf::st_sf()`](https://r-spatial.github.io/sf/reference/sf.html) places
the geometry columns last and doesn't support list columns or names that
clash with its arguments unless the input is a tibble, in these cases we
use the next constructor.
