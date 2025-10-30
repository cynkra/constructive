# Constructive options for class 'GuideAxis'

These options will be used on objects of class 'GuideAxis'.

## Usage

``` r
opts_GuideAxis(constructor = c("guide_axis", "next"), ...)
```

## Arguments

- constructor:

  String. Name of the function used to construct the object.

- ...:

  Additional options used by user defined constructors through the
  `opts` object

## Value

An object of class
\<constructive_options/constructive_options_GuideAxis\>

## Details

Depending on `constructor`, we construct the object as follows:

- `"guide_axis"` (default): We build the object using
  [`ggplot2::guide_axis()`](https://ggplot2.tidyverse.org/reference/guide_axis.html).

- `"next"` : Use the constructor for the next supported class.
