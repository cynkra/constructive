# Constructive options for class 'diagonalMatrix'

These options will be used on objects of class 'diagonalMatrix' from the
'Matrix' package, i.e. objects of classes 'ddiMatrix' or 'ldiMatrix'.

## Usage

``` r
opts_diagonalMatrix(constructor = c("Diagonal", "next"), ...)
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
\<constructive_options/constructive_options_diagonalMatrix\>

## Details

Depending on `constructor`, we construct the object as follows:

- `"Diagonal"` (default): We build the object using
  [`Matrix::Diagonal()`](https://rdrr.io/pkg/Matrix/man/Diagonal.html).
  If this cannot reproduce the object exactly (e.g. different row and
  column names) we fall back to the next constructor.

- `"next"` : Use the constructor for the next supported class. Call
  [`.class2()`](https://rdrr.io/r/base/class.html) on the object to see
  in which order the methods will be tried.
