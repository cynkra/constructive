# Constructive options for class 'GuideLegend'

These options will be used on objects of class 'GuideLegend'.

## Usage

``` r
opts_GuideLegend(constructor = c("guide_legend", "next"), ...)
```

## Arguments

- constructor:

  String. Name of the function used to construct the object.

- ...:

  Additional options used by user defined constructors through the
  `opts` object

## Value

An object of class
\<constructive_options/constructive_options_GuideLegend\>

## Details

Depending on `constructor`, we construct the object as follows:

- `"guide_legend"` (default): We build the object using
  [`ggplot2::guide_legend()`](https://ggplot2.tidyverse.org/reference/guide_legend.html).

- `"next"` : Use the constructor for the next supported class.
