# Constructive options for class 'ellmer::TypeJsonSchema\`'

These options will be used on objects of class
'ellmer::TypeJsonSchema\`'.

## Usage

``` r
opts_ellmer_TypeJsonSchema(
  constructor = c("type_from_schema", "TypeJsonSchema", "next"),
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
\<constructive_options/constructive_options_ellmer::TypeJsonSchema\`\>

## Details

Depending on `constructor`, we construct the object as follows:

- `"TypeJsonSchema"` (default): We build the object using
  [`ellmer::TypeJsonSchema()`](https://ellmer.tidyverse.org/reference/Type.html).

- `"next"` : Use the constructor for the next supported class.
