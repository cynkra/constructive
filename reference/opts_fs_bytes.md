# Constructive options for class 'fs_bytes'

These options will be used on objects of class 'fs_bytes'.

## Usage

``` r
opts_fs_bytes(constructor = c("as_fs_bytes", "next"), ...)
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
\<constructive_options/constructive_options_fs_bytes\>

## Details

Depending on `constructor`, we construct the object as follows:

- `"as_fs_bytes"` (default): Build the object using
  [`fs::as_fs_bytes()`](https://fs.r-lib.org/reference/fs_bytes.html) on
  a numeric vector.

- `"next"` : Use the constructor for the next supported class. Call
  [`.class2()`](https://rdrr.io/r/base/class.html) on the object to see
  in which order the methods will be tried.
