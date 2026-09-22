# Constructive options for class 'col_spec'

These options will be used on objects of class 'col_spec'.

## Usage

``` r
opts_col_spec(constructor = c("cols", "next", "list"), ...)
```

## Arguments

- constructor:

  String. Name of the function used to construct the object.

- ...:

  Additional options used by user defined constructors through the
  `opts` object

## Value

An object of class
\<constructive_options/constructive_options_col_spec\>

## Details

Depending on `constructor`, we construct the object as follows:

- `"cols"` (default): We build the object using
  [`readr::cols()`](https://readr.tidyverse.org/reference/cols.html), or
  [`readr::cols_only()`](https://readr.tidyverse.org/reference/cols.html)
  when the default collector is
  [`readr::col_skip()`](https://readr.tidyverse.org/reference/col_skip.html).
  The `.default` argument is omitted when it's
  [`readr::col_guess()`](https://readr.tidyverse.org/reference/parse_guess.html).

- `"next"` : Use the constructor for the next supported class. Call
  [`.class2()`](https://rdrr.io/r/base/class.html) on the object to see
  in which order the methods will be tried. This will usually be
  equivalent to `"list"`

- `"list"` : We define as a list and repair attributes

Use
[`opts_collector()`](https://cynkra.github.io/constructive/reference/opts_collector.md)
to tweak the construction of the column specifications.
