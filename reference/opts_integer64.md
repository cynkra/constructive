# Constructive options for class 'integer64'

These options will be used on objects of class 'integer64'.

## Usage

``` r
opts_integer64(constructor = c("as.integer64", "next", "double"), ...)
```

## Arguments

- constructor:

  String. Name of the function used to construct the object, see Details
  section.

- ...:

  Additional options used by user defined constructors through the
  `opts` object

## Value

An object of class
\<constructive_options/constructive_options_integer64\>

## Details

Depending on `constructor`, we construct the object as follows:

- `"as.integer64"` (default): Build the object using `as.integer64()` on
  a character vector.

- `"next"` : Use the constructor for the next supported class. Call
  [`.class2()`](https://rdrr.io/r/base/class.html) on the object to see
  in which order the methods will be tried.

- `"double"` : We define as an atomic vector and repair attributes.

We don't recommend the "next" and "double" constructors for this class
as they give incorrect results on negative or `NA` "integer64" objects
due to some quirks in the implementation of the 'bit64' package.
