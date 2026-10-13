# Constructive options for class 'sparseVector'

These options will be used on objects of class 'sparseVector' from the
'Matrix' package, i.e. objects of classes such as 'dsparseVector',
'isparseVector', 'lsparseVector', 'nsparseVector' or 'zsparseVector'.

## Usage

``` r
opts_sparseVector(constructor = c("sparseVector", "next"), ...)
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
\<constructive_options/constructive_options_sparseVector\>

## Details

Depending on `constructor`, we construct the object as follows:

- `"sparseVector"` (default): We build the object using
  [`Matrix::sparseVector()`](https://rdrr.io/pkg/Matrix/man/sparseVector.html).

- `"next"` : Use the constructor for the next supported class. Call
  [`.class2()`](https://rdrr.io/r/base/class.html) on the object to see
  in which order the methods will be tried.
