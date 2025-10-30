# Constructive options for class 'GuideCustom'

These options will be used on objects of class 'GuideCustom'.

## Usage

``` r
opts_GuideCustom(constructor = c("guide_custom", "next"), ...)
```

## Arguments

- constructor:

  String. Name of the function used to construct the object.

- ...:

  Additional options used by user defined constructors through the
  `opts` object

## Value

An object of class
\<constructive_options/constructive_options_GuideCustom\>

## Details

Depending on `constructor`, we construct the object as follows:

- `"guide_custom"` (default): We build the object using
  [`ggplot2::guide_custom()`](https://ggplot2.tidyverse.org/reference/guide_custom.html).

- `"next"` : Use the constructor for the next supported class.
