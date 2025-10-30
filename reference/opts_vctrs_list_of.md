# Constructive options for class 'data.table'

These options will be used on objects of class 'data.table'.

## Usage

``` r
opts_vctrs_list_of(constructor = c("list_of", "next", "list"), ...)
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
\<constructive_options/constructive_options_vctrs_list_of\>

## Details

Depending on `constructor`, we construct the object as follows:

- `"list_of"` (default): Wrap the column definitions in a `list_of()`
  call.

- `"list"` : Use [`list()`](https://rdrr.io/r/base/list.html) and treat
  the class as a regular attribute.
