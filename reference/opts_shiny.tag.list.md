# Constructive options for class 'shiny.tag.list'

These options will be used on objects of class 'shiny.tag.list'.

## Usage

``` r
opts_shiny.tag.list(constructor = c("tagList", "next"), ...)
```

## Arguments

- constructor:

  String. Name of the function used to construct the object.

- ...:

  Additional options used by user defined constructors through the
  `opts` object

## Value

An object of class
\<constructive_options/constructive_options_shiny.tag.list\>

## Details

Depending on `constructor`, we construct the object as follows:

- `"tagList"` (default): We build the object using
  [`htmltools::tagList()`](https://rstudio.github.io/htmltools/reference/tagList.html).

- `"next"` : Use the constructor for the next supported class.
