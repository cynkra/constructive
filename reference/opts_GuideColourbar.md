# Constructive options for class 'GuideColourbar'

These options will be used on objects of class 'GuideColourbar'.

## Usage

``` r
opts_GuideColourbar(constructor = c("guide_colorbar", "next"), ...)
```

## Arguments

- constructor:

  String. Name of the function used to construct the object.

- ...:

  Additional options used by user defined constructors through the
  `opts` object

## Value

An object of class
\<constructive_options/constructive_options_GuideColourbar\>

## Details

Depending on `constructor`, we construct the object as follows:

- `"guide_colorbar"` (default): We build the object using
  [`ggplot2::guide_colorbar()`](https://ggplot2.tidyverse.org/reference/guide_colourbar.html).

- `"next"` : Use the constructor for the next supported class.
