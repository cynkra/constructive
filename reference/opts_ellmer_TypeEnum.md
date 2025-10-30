# Constructive options for class 'ellmer::TypeEnum\`'

These options will be used on objects of class 'ellmer::TypeEnum\`'.

## Usage

``` r
opts_ellmer_TypeEnum(constructor = c("type_enum", "TypeEnum", "next"), ...)
```

## Arguments

- constructor:

  String. Name of the function used to construct the object.

- ...:

  Additional options used by user defined constructors through the
  `opts` object

## Value

An object of class
\<constructive_options/constructive_options_ellmer::TypeEnum\`\>

## Details

Depending on `constructor`, we construct the object as follows:

- `"TypeEnum"` (default): We build the object using
  [`ellmer::TypeEnum()`](https://ellmer.tidyverse.org/reference/Type.html).

- `"next"` : Use the constructor for the next supported class.
