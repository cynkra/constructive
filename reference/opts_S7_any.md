# Constructive options for class 'S7_any'

These options will be used on objects of class 'S7_any'.

## Usage

``` r
opts_S7_any(constructor = c("class_any", "next"), ...)
```

## Arguments

- constructor:

  String. Name of the function used to construct the object.

- ...:

  Additional options used by user defined constructors through the
  `opts` object

## Value

An object of class \<constructive_options/constructive_options_S7_any\>

## Details

Depending on `constructor`, we construct the object as follows:

- `"class_any"` (default): We build the object using
  [`S7::class_any()`](https://rconsortium.github.io/S7/reference/class_any.html).

- `"next"` : Use the constructor for the next supported class.
