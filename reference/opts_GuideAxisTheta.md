# Constructive options for class 'GuideAxisTheta'

These options will be used on objects of class 'GuideAxisTheta'.

## Usage

``` r
opts_GuideAxisTheta(constructor = c("guide_axis_theta", "next"), ...)
```

## Arguments

- constructor:

  String. Name of the function used to construct the object.

- ...:

  Additional options used by user defined constructors through the
  `opts` object

## Value

An object of class
\<constructive_options/constructive_options_GuideAxisTheta\>

## Details

Depending on `constructor`, we construct the object as follows:

- `"guide_axis_theta"` (default): We build the object using
  [`ggplot2::guide_axis_theta()`](https://ggplot2.tidyverse.org/reference/guide_axis_theta.html).

- `"next"` : Use the constructor for the next supported class.
