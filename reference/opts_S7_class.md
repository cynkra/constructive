# Constructive options for class 'S7_class'

These options will be used on objects of class 'S7_class'.

## Usage

``` r
opts_S7_class(constructor = c("new_class", "next"), ...)
```

## Arguments

- constructor:

  String. Name of the function used to construct the object.

- ...:

  Additional options used by user defined constructors through the
  `opts` object

## Value

An object of class
\<constructive_options/constructive_options_S7_class\>

## Details

Depending on `constructor`, we construct the object as follows:

- `"new_class"` (default): We build the object using
  [`S7::new_class()`](https://rconsortium.github.io/S7/reference/new_class.html).

- `"next"` : Use the constructor for the next supported class.
