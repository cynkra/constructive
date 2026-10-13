# Constructive options for class 'table'

These options will be used on objects of class 'table'.

## Usage

``` r
opts_table(constructor = c("as.table", "next"), ...)
```

## Arguments

- constructor:

  String. Name of the function used to construct the object, see Details
  section.

- ...:

  Additional options used by user defined constructors through the
  `opts` object

## Value

An object of class \<constructive_options/constructive_options_table\>

## Details

Depending on `constructor`, we construct the object as follows:

- `"as.table"` (default): We build the object using
  [`as.table()`](https://rdrr.io/r/base/table.html) on an array or a
  matrix. Objects of class "xtabs" are built this way too, their class
  and "call" attribute are then repaired.

- `"next"` : Use the constructor for the next supported class. Call
  [`.class2()`](https://rdrr.io/r/base/class.html) on the object to see
  in which order the methods will be tried. This will usually be
  equivalent to `opts_matrix("matrix")` or `opts_array("array")`.
