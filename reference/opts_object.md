# Constructive options for class 'object'

These options will be used on objects of class 'object'. The 'object'
type is particular in that it is an internal type in base R, but can
only be produced at time of writing through the 'S7' package. Well this
is not completely true since those can be built from S4 objects that we
remove the S4 flag from by using `asS3(x, complete = FALSE)` but we
don't propose this for now.

## Usage

``` r
opts_object(constructor = c("prototype", "S7_object"), ...)
```

## Arguments

- constructor:

  String. Name of the function used to construct the object.

- ...:

  Additional options used by user defined constructors through the
  `opts` object

## Value

An object of class \<constructive_options/constructive_options_object\>

## Details

Depending on `constructor`, we construct the object as follows:

- `"S7_object"` (default): We build the object using
  [`S7::S7_object()`](https://rconsortium.github.io/S7/reference/S7_object.html).
  At the time of writing, this is currently the only way to create these
  objects.

- `"next"` : Use the constructor for the next supported class.
