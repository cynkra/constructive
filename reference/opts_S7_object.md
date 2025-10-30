# Constructive options for class 'S7_object'

These options will be used on objects of class 'S7_object'. This class
is unusual in that it can be applied on objects of different internal
types, objects of length 1 class `"S7_object"` are of type `"object"`
while for instance objects of class `c("S7_class", "S7_object")` are
closures. For closures we fall back to the next method.

## Usage

``` r
opts_S7_object(constructor = c("S7_object", "next"), ...)
```

## Arguments

- constructor:

  String. Name of the function used to construct the object.

- ...:

  Additional options used by user defined constructors through the
  `opts` object

## Value

An object of class
\<constructive_options/constructive_options_S7_object\>

## Details

Depending on `constructor`, we construct the object as follows:

- `"S7_object"` (default): We build the object using
  [`S7::S7_object()`](https://rconsortium.github.io/S7/reference/S7_object.html).

- `"next"` : Use the constructor for the next supported class.

## Before the "object" type

The information above is correct only starting from R 4.4. The "object"
type was introduced in R 4.4 and the S7 package uses the "S4" type
instead for previous versions.
