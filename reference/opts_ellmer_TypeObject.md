# Constructive options for class 'ellmer::TypeObject\`'

These options will be used on objects of class 'ellmer::TypeObject\`'.

## Usage

``` r
opts_ellmer_TypeObject(
  constructor = c("type_object", "TypeObject", "next"),
  ...
)
```

## Arguments

- constructor:

  String. Name of the function used to construct the object.

- ...:

  Additional options used by user defined constructors through the
  `opts` object

## Value

An object of class
\<constructive_options/constructive_options_ellmer::TypeObject\`\>

## Details

Depending on `constructor`, we construct the object as follows:

- `"TypeObject"` (default): We build the object using
  [`ellmer::TypeObject()`](https://ellmer.tidyverse.org/reference/Type.html).

- `"next"` : Use the constructor for the next supported class.
