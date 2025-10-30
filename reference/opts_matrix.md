# Constructive options for matrices

Matrices are atomic vectors, lists, or objects of type `"expression"`
with a `"dim"` attributes of length 2.

## Usage

``` r
opts_matrix(constructor = c("matrix", "array", "cbind", "rbind", "next"), ...)
```

## Arguments

- constructor:

  String. Name of the function used to construct the object.

- ...:

  Additional options used by user defined constructors through the
  `opts` object

## Value

An object of class \<constructive_options/constructive_options_matrix\>

## Details

Depending on `constructor`, we construct the object as follows:

- `"matrix"` : We use [`matrix()`](https://rdrr.io/r/base/matrix.html)

- `"array"` : We use [`array()`](https://rdrr.io/r/base/array.html)

- `"cbind"`,`"rbind"` : We use
  [`cbind()`](https://rdrr.io/r/base/cbind.html) or `"rbind()"`, this
  makes named columns and rows easier to read.

- `"next"` : Use the constructor for the next supported class. Call
  [`.class2()`](https://rdrr.io/r/base/class.html) on the object to see
  in which order the methods will be tried. This will usually be
  equivalent to `"array"`

- `"atomic"` : We define as an atomic vector and repair attributes
