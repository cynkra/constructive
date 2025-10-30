# Constructive options for arrays

These options will be used on arrays. Note that arrays can be built on
top of vectors, lists or expressions. Canonical arrays have an implicit
class "array" shown by [`class()`](https://rdrr.io/r/base/class.html)
but "array" is not part of the class attribute.

## Usage

``` r
opts_array(constructor = c("array", "next"), ...)
```

## Arguments

- constructor:

  String. Name of the function used to construct the object, see Details
  section.

- ...:

  Additional options used by user defined constructors through the
  `opts` object

## Value

An object of class \<constructive_options/constructive_options_array\>

## Details

Depending on `constructor`, we construct the object as follows:

- `"array"` (default): Use the
  [`array()`](https://rdrr.io/r/base/array.html) function

- `"next"` : Use the constructor for the next supported class. Call
  [`.class2()`](https://rdrr.io/r/base/class.html) on the object to see
  in which order the methods will be tried.
