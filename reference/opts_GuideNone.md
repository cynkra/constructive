# Constructive options for class 'GuideNone'

These options will be used on objects of class 'GuideNone'.

## Usage

``` r
opts_GuideNone(constructor = c("guide_none", "next"), ...)
```

## Arguments

- constructor:

  String. Name of the function used to construct the object.

- ...:

  Additional options used by user defined constructors through the
  `opts` object

## Value

An object of class
\<constructive_options/constructive_options_GuideNone\>

## Details

Depending on `constructor`, we construct the object as follows:

- `"guide_none"` (default): We build the object using
  [`ggplot2::guide_none()`](https://ggplot2.tidyverse.org/reference/guide_none.html).

- `"next"` : Use the constructor for the next supported class.
