# Constructive options for class 'haven_labelled_spss'

These options will be used on objects of class 'haven_labelled_spss'.

## Usage

``` r
opts_haven_labelled_spss(constructor = c("labelled_spss", "next"), ...)
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
\<constructive_options/constructive_options_haven_labelled_spss\>

## Details

Depending on `constructor`, we construct the object as follows:

- `"labelled_spss"` (default): We build the object using
  [`haven::labelled_spss()`](https://haven.tidyverse.org/reference/labelled_spss.html),
  the `labels`, `na_values`, `na_range` and `label` arguments are
  omitted when `NULL`.

- `"next"` : Use the constructor for the next supported class. Call
  [`.class2()`](https://rdrr.io/r/base/class.html) on the object to see
  in which order the methods will be tried.
