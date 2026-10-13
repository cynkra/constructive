# Constructive options for class 'html_dependency'

These options will be used on objects of class 'html_dependency'.

## Usage

``` r
opts_html_dependency(constructor = c("htmlDependency", "next"), ...)
```

## Arguments

- constructor:

  String. Name of the function used to construct the object.

- ...:

  Additional options used by user defined constructors through the
  `opts` object

## Value

An object of class
\<constructive_options/constructive_options_html_dependency\>

## Details

Depending on `constructor`, we construct the object as follows:

- `"htmlDependency"` (default): We build the object using
  [`htmltools::htmlDependency()`](https://rstudio.github.io/htmltools/reference/htmlDependency.html),
  arguments left to their default values are omitted.

- `"next"` : Use the constructor for the next supported class.
