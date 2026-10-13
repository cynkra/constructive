# Constructive options for class 'denseMatrix'

These options will be used on objects of class 'denseMatrix' from the
'Matrix' package, i.e. objects of classes such as 'dgeMatrix',
'lgeMatrix', 'dsyMatrix' or 'dtrMatrix'.

## Usage

``` r
opts_denseMatrix(constructor = c("Matrix", "next"), ...)
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
\<constructive_options/constructive_options_denseMatrix\>

## Details

Depending on `constructor`, we construct the object as follows:

- `"Matrix"` (default): We build the object using
  [`Matrix::Matrix()`](https://rdrr.io/pkg/Matrix/man/Matrix.html) on a
  base matrix, with `sparse = FALSE`.
  [`Matrix::Matrix()`](https://rdrr.io/pkg/Matrix/man/Matrix.html)
  detects symmetric and triangular matrices so if this cannot reproduce
  the object exactly (e.g. a general matrix with symmetric content,
  packed storage, cached factorizations) we fall back to the next
  constructor.

- `"next"` : Use the constructor for the next supported class. Call
  [`.class2()`](https://rdrr.io/r/base/class.html) on the object to see
  in which order the methods will be tried.
