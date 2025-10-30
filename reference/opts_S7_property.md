# Constructive options for class 'S7_property'

These options will be used on objects of class 'S7_property'.

## Usage

``` r
opts_S7_property(constructor = c("new_property", "next"), ...)
```

## Arguments

- constructor:

  String. Name of the function used to construct the object.

- ...:

  Additional options used by user defined constructors through the
  `opts` object

## Value

An object of class
\<constructive_options/constructive_options_S7_property\>

## Details

Depending on `constructor`, we construct the object as follows:

- `"new_property"` (default): We build the object using
  [`S7::new_property()`](https://rconsortium.github.io/S7/reference/new_property.html).

- `"next"` : Use the constructor for the next supported class.
