# Constructive options for class 'tbl_ts'

These options will be used on objects of class 'tbl_ts', also known as
tsibbles.

## Usage

``` r
opts_tbl_ts(constructor = c("tsibble", "as_tsibble", "next"), ...)
```

## Arguments

- constructor:

  String. Name of the function used to construct the object, see Details
  section.

- ...:

  Additional options used by user defined constructors through the
  `opts` object

## Value

An object of class \<constructive_options/constructive_options_tbl_ts\>

## Details

Depending on `constructor`, we construct the object as follows:

- `"tsibble"` (default): Wrap the column definitions in a
  [`tsibble::tsibble()`](https://tsibble.tidyverts.org/reference/tsibble.html)
  call. If some column names conflict with the arguments of
  [`tsibble::tsibble()`](https://tsibble.tidyverts.org/reference/tsibble.html)
  we fall back to `"as_tsibble"`.

- `"as_tsibble"` : Construct the underlying tibble and pipe it into
  [`tsibble::as_tsibble()`](https://tsibble.tidyverts.org/reference/as-tsibble.html).
  Use
  [`opts_tbl_df()`](https://cynkra.github.io/constructive/reference/opts_tbl_df.md)
  to tweak the construction of the tibble.

- `"next"` : Use the constructor for the next supported class. Call
  [`.class2()`](https://rdrr.io/r/base/class.html) on the object to see
  in which order the methods will be tried.

The "interval" attribute is always recomputed by 'tsibble', we only set
`regular = FALSE` for irregular tsibbles.
