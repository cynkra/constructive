# Constructive options for class 'S7_class_ref'

These options will be used on objects of class 'S7_class_ref'. Since S7
0.2.2.9000 an S7 object stores its class in the "\_S7_class" attribute
through such a reference, which
[`S7::new_class()`](https://rconsortium.github.io/S7/reference/new_class.html)
creates and stores in the environment of the class.

## Usage

``` r
opts_S7_class_ref(constructor = c("environment", "next"), ...)
```

## Arguments

- constructor:

  String. Name of the function used to construct the object.

- ...:

  Additional options used by user defined constructors through the
  `opts` object

## Value

An object of class
\<constructive_options/constructive_options_S7_class_ref\>

## Details

Depending on `constructor`, we construct the object as follows:

- `"environment"` (default): We build the class and fetch the reference
  from its environment.

- `"next"` : Use the constructor for the next supported class.
