# Constructive options for class 'GuideBins'

These options will be used on objects of class 'GuideBins'.

## Usage

``` r
opts_GuideBins(constructor = c("guide_bins", "next"), ...)
```

## Arguments

- constructor:

  String. Name of the function used to construct the object.

- ...:

  Additional options used by user defined constructors through the
  `opts` object

## Value

An object of class
\<constructive_options/constructive_options_GuideBins\>

## Details

Depending on `constructor`, we construct the object as follows:

- `"guide_bins"` (default): We build the object using
  [`ggplot2::guide_bins()`](https://ggplot2.tidyverse.org/reference/guide_bins.html).

- `"next"` : Use the constructor for the next supported class.
