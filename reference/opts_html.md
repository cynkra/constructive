# Constructive options for class 'html'

These options will be used on objects of class 'html'.

## Usage

``` r
opts_html(constructor = c("HTML", "next"), ...)
```

## Arguments

- constructor:

  String. Name of the function used to construct the object.

- ...:

  Additional options used by user defined constructors through the
  `opts` object

## Value

An object of class \<constructive_options/constructive_options_html\>

## Details

Depending on `constructor`, we construct the object as follows:

- `"HTML"` (default): We build the object using
  [`htmltools::HTML()`](https://rstudio.github.io/htmltools/reference/HTML.html).

- `"next"` : Use the constructor for the next supported class.
