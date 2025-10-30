# Constructive options for class 'S7_base_class'

These options will be used on objects of class 'S7_base_class'.

## Usage

``` r
opts_S7_base_class(constructor = c("S7", "next"), ...)
```

## Arguments

- constructor:

  String. Name of the function used to construct the object.

- ...:

  Additional options used by user defined constructors through the
  `opts` object

## Value

An object of class
\<constructive_options/constructive_options_S7_base_class\>

## Details

Depending on `constructor`, we construct the object as follows:

- `"S7"` (default): We build the object using `S7()`.

- `"next"` : Use the constructor for the next supported class.
