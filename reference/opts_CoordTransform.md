# Constructive options for class 'CoordTransform'

These options will be used on objects of class 'CoordTransform'.

## Usage

``` r
opts_CoordTransform(constructor = c("coord_transform", "next"), ...)
```

## Arguments

- constructor:

  String. Name of the function used to construct the object.

- ...:

  Additional options used by user defined constructors through the
  `opts` object

## Value

An object of class
\<constructive_options/constructive_options_CoordTransform\>

## Details

Depending on `constructor`, we construct the object as follows:

- `"coord_transform"` (default): We build the object using
  [`ggplot2::coord_transform()`](https://ggplot2.tidyverse.org/reference/coord_transform.html).

- `"next"` : Use the constructor for the next supported class.
