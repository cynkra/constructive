# Constructive options for class 'data.frame'

These options will be used on objects of class 'data.frame'.

## Usage

``` r
opts_data.frame(
  constructor = c("data.frame", "read.table", "next", "list"),
  ...,
  recycle = TRUE
)
```

## Arguments

- constructor:

  String. Name of the function used to construct the object, see Details
  section.

- ...:

  Additional options used by user defined constructors through the
  `opts` object

- recycle:

  Boolean. For the `"data.frame"` constructor. Whether to recycle
  scalars to compress the output.

## Value

An object of class
\<constructive_options/constructive_options_data.frame\>

## Details

Depending on `constructor`, we construct the object as follows:

- `"data.frame"` (default): Wrap the column definitions in a
  [`data.frame()`](https://rdrr.io/r/base/data.frame.html) call. If some
  columns are lists or data frames, we wrap the column definitions in
  [`tibble::tibble()`](https://tibble.tidyverse.org/reference/tibble.html).
  then use
  [`as.data.frame()`](https://rdrr.io/r/base/as.data.frame.html).

- `"read.table"` : We build the object using
  [`read.table()`](https://rdrr.io/r/utils/read.table.html) if possible,
  or fall back to
  [`data.frame()`](https://rdrr.io/r/base/data.frame.html).

- `"next"` : Use the constructor for the next supported class. Call
  [`.class2()`](https://rdrr.io/r/base/class.html) on the object to see
  in which order the methods will be tried.

- `"list"` : Use [`list()`](https://rdrr.io/r/base/list.html) and treat
  the class as a regular attribute.
