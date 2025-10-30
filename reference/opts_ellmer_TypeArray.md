# Constructive options for class 'ellmer::TypeArray\`'

These options will be used on objects of class 'ellmer::TypeArray\`'.

## Usage

``` r
opts_ellmer_TypeArray(constructor = c("type_array", "TypeArray", "next"), ...)
```

## Arguments

- constructor:

  String. Name of the function used to construct the object.

- ...:

  Additional options used by user defined constructors through the
  `opts` object

## Value

An object of class
\<constructive_options/constructive_options_ellmer::TypeArray\`\>

## Details

Depending on `constructor`, we construct the object as follows:

- `"TypeArray"` (default): We build the object using
  [`ellmer::TypeArray()`](https://ellmer.tidyverse.org/reference/Type.html).

- `"next"` : Use the constructor for the next supported class.
