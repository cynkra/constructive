# Constructive options for class 'ellmer::TypeBasic\`'

These options will be used on objects of class 'ellmer::TypeBasic\`'.

## Usage

``` r
opts_ellmer_TypeBasic(constructor = c("default", "TypeBasic", "next"), ...)
```

## Arguments

- constructor:

  String. Name of the function used to construct the object.

- ...:

  Additional options used by user defined constructors through the
  `opts` object

## Value

An object of class
\<constructive_options/constructive_options_ellmer::TypeBasic\`\>

## Details

Depending on `constructor`, we construct the object as follows:

- `"TypeBasic"` (default): We build the object using
  [`ellmer::TypeBasic()`](https://ellmer.tidyverse.org/reference/Type.html).

- `"next"` : Use the constructor for the next supported class.
