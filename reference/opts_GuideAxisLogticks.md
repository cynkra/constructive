# Constructive options for class 'GuideAxisLogticks'

These options will be used on objects of class 'GuideAxisLogticks'.

## Usage

``` r
opts_GuideAxisLogticks(constructor = c("guide_axis_logticks", "next"), ...)
```

## Arguments

- constructor:

  String. Name of the function used to construct the object.

- ...:

  Additional options used by user defined constructors through the
  `opts` object

## Value

An object of class
\<constructive_options/constructive_options_GuideAxisLogticks\>

## Details

Depending on `constructor`, we construct the object as follows:

- `"guide_axis_logticks"` (default): We build the object using
  [`ggplot2::guide_axis_logticks()`](https://ggplot2.tidyverse.org/reference/guide_axis_logticks.html).

- `"next"` : Use the constructor for the next supported class.
