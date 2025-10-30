# Constructive options for class 'GuideAxisStack'

These options will be used on objects of class 'GuideAxisStack'.

## Usage

``` r
opts_GuideAxisStack(constructor = c("guide_axis_stack", "next"), ...)
```

## Arguments

- constructor:

  String. Name of the function used to construct the object.

- ...:

  Additional options used by user defined constructors through the
  `opts` object

## Value

An object of class
\<constructive_options/constructive_options_GuideAxisStack\>

## Details

Depending on `constructor`, we construct the object as follows:

- `"guide_axis_stack"` (default): We build the object using
  [`ggplot2::guide_axis_stack()`](https://ggplot2.tidyverse.org/reference/guide_axis_stack.html).

- `"next"` : Use the constructor for the next supported class.
