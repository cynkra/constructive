# Constructive options for class 'quosure'

These options will be used on objects of class 'quosure'.

## Usage

``` r
opts_quosure(constructor = c("new_quosure", "next", "language"), ...)
```

## Arguments

- constructor:

  String. Name of the function used to construct the object, see Details
  section.

- ...:

  Additional options used by user defined constructors through the
  `opts` object

## Value

An object of class \<constructive_options/constructive_options_quosure\>

## Details

Depending on `constructor`, we construct the object as follows:

- `"new_quosure"` (default): Build the object using a `new_quosure()`
  call on a character vector.

- `"next"` : Use the constructor for the next supported class. Call
  [`.class2()`](https://rdrr.io/r/base/class.html) on the object to see
  in which order the methods will be tried.

- `"language"` : We define as an language object and repair attributes.
