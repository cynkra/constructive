# Constructive options for class 'FacetNull'

These options will be used on objects of class 'FacetNull'.

## Usage

``` r
opts_FacetNull(constructor = c("facet_null", "next"), ...)
```

## Arguments

- constructor:

  String. Name of the function used to construct the object.

- ...:

  Additional options used by user defined constructors through the
  `opts` object

## Value

An object of class
\<constructive_options/constructive_options_FacetNull\>

## Details

Depending on `constructor`, we construct the object as follows:

- `"facet_null"` (default): We build the object using
  [`ggplot2::facet_null()`](https://ggplot2.tidyverse.org/reference/facet_null.html).

- `"next"` : Use the constructor for the next supported class.
