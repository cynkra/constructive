# Constructive options for class 'shiny.tag.function'

These options will be used on objects of class 'shiny.tag.function'.

## Usage

``` r
opts_shiny.tag.function(constructor = c("tagFunction", "next"), ...)
```

## Arguments

- constructor:

  String. Name of the function used to construct the object.

- ...:

  Additional options used by user defined constructors through the
  `opts` object

## Value

An object of class
\<constructive_options/constructive_options_shiny.tag.function\>

## Details

Depending on `constructor`, we construct the object as follows:

- `"tagFunction"` (default): We build the object using
  [`htmltools::tagFunction()`](https://rstudio.github.io/htmltools/reference/tagFunction.html).

- `"next"` : Use the constructor for the next supported class.

Use
[`opts_function()`](https://cynkra.github.io/constructive/reference/opts_function.md)
to tweak the construction of the function.
