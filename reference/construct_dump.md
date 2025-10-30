# Dump Constructed Code to a File

An alternative to [`base::dump()`](https://rdrr.io/r/base/dump.html)
using code built with constructive.

## Usage

``` r
construct_dump(x, path, append = FALSE, ...)
```

## Arguments

- x:

  A named list or an environment.

- path:

  File or connection to write to.

- append:

  If FALSE, will overwrite existing file. If TRUE, will append to
  existing file. In both cases, if the file does not exist a new file is
  created.

- ...:

  Forwarded to
  [`construct_multi()`](https://cynkra.github.io/constructive/reference/construct.md)

## Value

Returns `NULL` invisibly, called for side effects.
