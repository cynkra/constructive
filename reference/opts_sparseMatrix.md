# Constructive options for class 'sparseMatrix'

These options will be used on objects of class 'sparseMatrix' from the
'Matrix' package in compressed column, compressed row or triplet form,
i.e. objects of classes such as 'dgCMatrix', 'dgRMatrix', 'dgTMatrix',
'lgCMatrix', 'ngCMatrix', 'dsCMatrix' or 'dtCMatrix'.

## Usage

``` r
opts_sparseMatrix(constructor = c("sparseMatrix", "next"), ...)
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
\<constructive_options/constructive_options_sparseMatrix\>

## Details

Depending on `constructor`, we construct the object as follows:

- `"sparseMatrix"` (default): We build the object using
  [`Matrix::sparseMatrix()`](https://rdrr.io/pkg/Matrix/man/sparseMatrix.html)
  on the (1-based) row and column indices of the stored entries. If this
  cannot reproduce the object exactly (e.g. cached factorizations, unit
  triangular matrices) we fall back to the next constructor.

- `"next"` : Use the constructor for the next supported class. Call
  [`.class2()`](https://rdrr.io/r/base/class.html) on the object to see
  in which order the methods will be tried.
