# Constructive options for tibbles

These options will be used on objects of class 'tbl_df', also known as
tibbles.

## Usage

``` r
opts_tbl_df(
  constructor = c("tibble", "tribble", "next", "list"),
  ...,
  trailing_comma = TRUE,
  justify = c("left", "right", "centre", "none"),
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

- trailing_comma:

  Boolean. Whether to leave a trailing comma at the end of the
  constructor call calls

- justify:

  String. Justification for columns if `constructor` is `"tribble"`

- recycle:

  Boolean. For the `"tibble"` constructor. Whether to recycle scalars to
  compress the output.

## Value

An object of class \<constructive_options/constructive_options_tbl_df\>

## Details

Depending on `constructor`, we construct the object as follows:

- `"tibble"` (default): Wrap the column definitions in a
  [`tibble::tibble()`](https://tibble.tidyverse.org/reference/tibble.html)
  call.

- `"tribble"` : We build the object using
  [`tibble::tribble()`](https://tibble.tidyverse.org/reference/tribble.html)
  if possible, and fall back to
  [`tibble::tibble()`](https://tibble.tidyverse.org/reference/tibble.html).

- `"next"` : Use the constructor for the next supported class. Call
  [`.class2()`](https://rdrr.io/r/base/class.html) on the object to see
  in which order the methods will be tried.

- `"list"` : Use [`list()`](https://rdrr.io/r/base/list.html) and treat
  the class as a regular attribute.
