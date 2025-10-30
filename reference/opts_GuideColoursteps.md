# Constructive options for class 'GuideColoursteps'

These options will be used on objects of class 'GuideColoursteps'.

## Usage

``` r
opts_GuideColoursteps(constructor = c("guide_colorsteps", "next"), ...)
```

## Arguments

- constructor:

  String. Name of the function used to construct the object.

- ...:

  Additional options used by user defined constructors through the
  `opts` object

## Value

An object of class
\<constructive_options/constructive_options_GuideColoursteps\>

## Details

Depending on `constructor`, we construct the object as follows:

- `"guide_colorsteps"` (default): We build the object using
  [`ggplot2::guide_colorsteps()`](https://ggplot2.tidyverse.org/reference/guide_coloursteps.html).

- `"next"` : Use the constructor for the next supported class.
