# Constructive options for class 'indMatrix'

These options will be used on objects of class 'indMatrix' from the
'Matrix' package, including objects of its subclass 'pMatrix'.

## Usage

``` r
opts_indMatrix(constructor = c("as", "next"), ...)
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
\<constructive_options/constructive_options_indMatrix\>

## Details

Depending on `constructor`, we construct the object as follows:

- `"as"` (default): We build the object using `as()` on the `perm`
  integer vector, or on a list of `perm` and the number of columns when
  it cannot be inferred, and apply `Matrix::t()` if the object's
  `margin` is 2. If this cannot reproduce the object exactly (e.g.
  dimnames) we fall back to the next constructor.

- `"next"` : Use the constructor for the next supported class. Call
  [`.class2()`](https://rdrr.io/r/base/class.html) on the object to see
  in which order the methods will be tried.
