# Constructive options for class 'CoordRadial'

These options will be used on objects of class 'CoordRadial'.

## Usage

``` r
opts_CoordRadial(constructor = c("coord_radial", "next"), ...)
```

## Arguments

- constructor:

  String. Name of the function used to construct the object.

- ...:

  Additional options used by user defined constructors through the
  `opts` object

## Value

An object of class
\<constructive_options/constructive_options_CoordRadial\>

## Details

Depending on `constructor`, we construct the object as follows:

- `"coord_radial"` (default): We build the object using
  [`ggplot2::coord_radial()`](https://ggplot2.tidyverse.org/reference/coord_radial.html).

- `"next"` : Use the constructor for the next supported class.
