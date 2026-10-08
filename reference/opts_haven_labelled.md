# Constructive options for class 'haven_labelled'

These options will be used on objects of class 'haven_labelled'.

## Usage

``` r
opts_haven_labelled(constructor = c("labelled", "next"), ...)
```

## Arguments

- constructor:

  String. Name of the function used to construct the object, see Details
  section.

- ...:

  Additional options used by user defined constructors through the
  `opts` object

## Value

An object of class
\<constructive_options/constructive_options_haven_labelled\>

## Details

Depending on `constructor`, we construct the object as follows:

- `"labelled"` (default): We build the object using
  [`haven::labelled()`](https://haven.tidyverse.org/reference/labelled.html),
  the `labels` and `label` arguments are omitted when `NULL`.

- `"next"` : Use the constructor for the next supported class. Call
  [`.class2()`](https://rdrr.io/r/base/class.html) on the object to see
  in which order the methods will be tried.
