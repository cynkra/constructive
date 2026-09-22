# Constructive options for class 'glue'

These options will be used on objects of class 'glue'.

## Usage

``` r
opts_glue(constructor = c("as_glue", "next"), ...)
```

## Arguments

- constructor:

  String. Name of the function used to construct the object, see Details
  section.

- ...:

  Additional options used by user defined constructors through the
  `opts` object

## Value

An object of class \<constructive_options/constructive_options_glue\>

## Details

Depending on `constructor`, we construct the object as follows:

- `"as_glue"` (default): Use
  [`glue::as_glue()`](https://glue.tidyverse.org/reference/as_glue.html)
  on a character vector.

- `"next"` : Use the constructor for the next supported class. Call
  [`.class2()`](https://rdrr.io/r/base/class.html) on the object to see
  in which order the methods will be tried.

Use
[`opts_character()`](https://cynkra.github.io/constructive/reference/opts_character.md)
to tweak the construction of the character vector constructed as part of
the glue construction.
