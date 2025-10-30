# Constructive options for class 'ggplot2::labels'

These options will be used on objects of class 'ggplot2::labels'.

## Usage

``` r
opts_ggplot2_labels(constructor = c("labs", "next"), ...)
```

## Arguments

- constructor:

  String. Name of the function used to construct the object.

- ...:

  Additional options used by user defined constructors through the
  `opts` object

## Value

An object of class
\<constructive_options/constructive_options_ggplot2::labels\>

## Details

Depending on `constructor`, we construct the object as follows:

- `"labs"` (default): We build the object using
  [`ggplot2::labs()`](https://ggplot2.tidyverse.org/reference/labs.html).

- `"next"` : Use the constructor for the next supported class.
