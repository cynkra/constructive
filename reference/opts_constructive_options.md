# Constructive options for the class `constructive_options`

These options will be used on objects of class `constructive_options`.

## Usage

``` r
opts_constructive_options(constructor = c("opts", "next"), ...)
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
\<constructive_options/constructive_options_constructive_options\>

## Details

Depending on `constructor`, we construct the object as follows:

- `"opts"` : Use the relevant `constructive::opts_?()` function.

- `"next"` : Use the constructor for the next supported class. Call
  [`.class2()`](https://rdrr.io/r/base/class.html) on the object to see
  in which order the methods will be tried.
