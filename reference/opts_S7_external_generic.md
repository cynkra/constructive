# Constructive options for class 'S7_external_generic'

These options will be used on objects of class 'S7_external_generic'.

## Usage

``` r
opts_S7_external_generic(constructor = c("new_external_generic", "next"), ...)
```

## Arguments

- constructor:

  String. Name of the function used to construct the object.

- ...:

  Additional options used by user defined constructors through the
  `opts` object

## Value

An object of class
\<constructive_options/constructive_options_S7_external_generic\>

## Details

Depending on `constructor`, we construct the object as follows:

- `"new_external_generic"` (default): We build the object using
  [`S7::new_external_generic()`](https://rconsortium.github.io/S7/reference/new_external_generic.html).

- `"next"` : Use the constructor for the next supported class.
