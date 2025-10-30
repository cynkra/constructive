# Constructive options for class 'ggplot'

These options will be used on objects of class 'ggplot'.

## Usage

``` r
opts_ggplot(constructor = c("ggplot", "next", "list"), ...)
```

## Arguments

- constructor:

  String. Name of the function used to construct the object, see Details
  section.

- ...:

  Additional options used by user defined constructors through the
  `opts` object

## Value

An object of class \<constructive_options/constructive_options_ggplot\>

## Details

Depending on `constructor`, we construct the object as follows:

- `"ggplot"` (default): Use
  [`ggplot2::ggplot()`](https://ggplot2.tidyverse.org/reference/ggplot.html)

- `"next"` : Use the constructor for the next supported class. Call
  [`.class2()`](https://rdrr.io/r/base/class.html) on the object to see
  in which order the methods will be tried.

- `"list"` : Use [`list()`](https://rdrr.io/r/base/list.html) and treat
  the class as a regular attribute.
