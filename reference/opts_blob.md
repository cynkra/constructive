# Constructive options for class 'blob'

These options will be used on objects of class 'blob'.

## Usage

``` r
opts_blob(constructor = c("blob", "next"), ...)
```

## Arguments

- constructor:

  String. Name of the function used to construct the object.

- ...:

  Additional options used by user defined constructors through the
  `opts` object

## Value

An object of class \<constructive_options/constructive_options_blob\>

## Details

Depending on `constructor`, we construct the object as follows:

- `"blob"` (default): Use
  [`blob::blob()`](https://blob.tidyverse.org/reference/blob.html) on a
  raw object.

  - `"new_blob"` (default): Use
    [`blob::new_blob()`](https://blob.tidyverse.org/reference/blob.html)
    on a list of raw objects.

- `"as.blob"` : Use
  [`blob::as_blob()`](https://blob.tidyverse.org/reference/blob.html) on
  a character vector

Use
[`opts_raw()`](https://cynkra.github.io/constructive/reference/opts_raw.md)
and
[`opts_character()`](https://cynkra.github.io/constructive/reference/opts_character.md)
to tweak the construction of raw or character objects constructed as
part of the blob construction.
