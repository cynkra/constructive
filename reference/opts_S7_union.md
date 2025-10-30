# Constructive options for class 'S7_union'

These options will be used on objects of class 'S7_union'.

## Usage

``` r
opts_S7_union(constructor = c("default", "|", "new_union", "next"), ...)
```

## Arguments

- constructor:

  String. Name of the function used to construct the object.

- ...:

  Additional options used by user defined constructors through the
  `opts` object

## Value

An object of class
\<constructive_options/constructive_options_S7_union\>

## Details

Depending on `constructor`, we construct the object as follows:

- `"default"` : We build the object using its name if it's a
  "class_union" object provided by the 'S7' package, or fall back to the
  `"|"` constructor.

- `"|"` : We build the object using the `|` operator.

- `"new_union"` (default): We build the object using
  [`S7::new_union()`](https://rconsortium.github.io/S7/reference/new_union.html).

- `"next"` : Use the constructor for the next supported class.
