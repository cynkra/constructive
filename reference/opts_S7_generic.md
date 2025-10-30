# Constructive options for class 'S7_generic'

These options will be used on objects of class 'S7_generic'.

## Usage

``` r
opts_S7_generic(constructor = c("new_generic", "next"), ...)
```

## Arguments

- constructor:

  String. Name of the function used to construct the object.

- ...:

  Additional options used by user defined constructors through the
  `opts` object

## Value

An object of class
\<constructive_options/constructive_options_S7_generic\>

## Details

Depending on `constructor`, we construct the object as follows:

- `"new_generic"` (default): We build the object using
  [`S7::new_generic()`](https://rconsortium.github.io/S7/reference/new_generic.html).

- `"next"` : Use the constructor for the next supported class.
