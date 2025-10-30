# Constructive options for class 'Guides'

These options will be used on objects of class 'Guides'.

## Usage

``` r
opts_Guides(constructor = c("guides", "next"), ...)
```

## Arguments

- constructor:

  String. Name of the function used to construct the object.

- ...:

  Additional options used by user defined constructors through the
  `opts` object

## Value

An object of class \<constructive_options/constructive_options_Guides\>

## Details

Depending on `constructor`, we construct the object as follows:

- `"guides"` (default): We build the object using
  [`ggplot2::guides()`](https://ggplot2.tidyverse.org/reference/guides.html).

- `"next"` : Use the constructor for the next supported class.
