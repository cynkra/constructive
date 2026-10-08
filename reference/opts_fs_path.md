# Constructive options for class 'fs_path'

These options will be used on objects of class 'fs_path'.

## Usage

``` r
opts_fs_path(constructor = c("path", "as_fs_path", "next"), ...)
```

## Arguments

- constructor:

  String. Name of the function used to construct the object, see Details
  section.

- ...:

  Additional options used by user defined constructors through the
  `opts` object

## Value

An object of class \<constructive_options/constructive_options_fs_path\>

## Details

Depending on `constructor`, we construct the object as follows:

- `"path"` (default): Build the object using
  [`fs::path()`](https://fs.r-lib.org/reference/path.html) on a
  character vector.

- `"as_fs_path"` : Build the object using
  [`fs::as_fs_path()`](https://fs.r-lib.org/reference/fs_path.html) on a
  character vector.

- `"next"` : Use the constructor for the next supported class. Call
  [`.class2()`](https://rdrr.io/r/base/class.html) on the object to see
  in which order the methods will be tried.

Both [`fs::path()`](https://fs.r-lib.org/reference/path.html) and
[`fs::as_fs_path()`](https://fs.r-lib.org/reference/fs_path.html) tidy
their input, so paths that are not tidy (e.g. `"a//b/"`) are always
constructed with the `"next"` constructor.
