# Constructive options for class 'fs_perms'

These options will be used on objects of class 'fs_perms'.

## Usage

``` r
opts_fs_perms(constructor = c("as_fs_perms", "next"), ...)
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
\<constructive_options/constructive_options_fs_perms\>

## Details

Depending on `constructor`, we construct the object as follows:

- `"as_fs_perms"` (default): Build the object using
  [`fs::as_fs_perms()`](https://fs.r-lib.org/reference/fs_perms.html) on
  octal strings such as `"644"`. Vectors of length other than 1 or 2 are
  first converted with
  [`as.octmode()`](https://rdrr.io/r/base/octmode.html), because
  [`fs::as_fs_perms()`](https://fs.r-lib.org/reference/fs_perms.html)
  scrambles the order of longer character vectors.

- `"next"` : Use the constructor for the next supported class. Call
  [`.class2()`](https://rdrr.io/r/base/class.html) on the object to see
  in which order the methods will be tried.

Objects containing `NA` or negative values are always constructed with
the `"next"` constructor.
