# Constructive options for class 'grouped_ts'

These options will be used on objects of class 'grouped_ts'.

## Usage

``` r
opts_grouped_ts(constructor = c("group_by", "next"), ...)
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
\<constructive_options/constructive_options_grouped_ts\>

## Details

Depending on `constructor`, we construct the object as follows:

- `"group_by"` (default): Construct the ungrouped tsibble and pipe it
  into
  [`dplyr::group_by()`](https://dplyr.tidyverse.org/reference/group_by.html),
  and into
  [`tsibble::index_by()`](https://tsibble.tidyverts.org/reference/index-by.html)
  if the object is grouped by an index. Use
  [`opts_tbl_ts()`](https://cynkra.github.io/constructive/reference/opts_tbl_ts.md)
  to tweak the construction of the tsibble.

- `"next"` : Use the constructor for the next supported class. Call
  [`.class2()`](https://rdrr.io/r/base/class.html) on the object to see
  in which order the methods will be tried.
