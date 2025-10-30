# Constructive options for class 'quosures'

These options will be used on objects of class 'quosures'.

## Usage

``` r
opts_quosures(constructor = c("new_quosures", "next", "list"), ...)
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
\<constructive_options/constructive_options_quosures\>

## Details

Depending on `constructor`, we construct the object as follows:

- `"as_quosures"` (default): Build the object using a `as_quosures()`
  call on a character vector.

- `"next"` : Use the constructor for the next supported class. Call
  [`.class2()`](https://rdrr.io/r/base/class.html) on the object to see
  in which order the methods will be tried.

- `"list"` : We define as an list object and repair attributes.
