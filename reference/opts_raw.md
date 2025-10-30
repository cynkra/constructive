# Constructive options for type 'raw'

These options will be used on objects of type 'raw'.

Depending on `constructor`, we construct the object as follows:

- `"as.raw"` (default): Use
  [`as.raw()`](https://rdrr.io/r/base/raw.html), or
  [`raw()`](https://rdrr.io/r/base/raw.html) when relevant

- `"charToRaw"` : Use
  [`charToRaw()`](https://rdrr.io/r/base/rawConversion.html) on a
  string, if the a raw vector contains a zero we fall back to the
  "as.raw" constructor.

To set options on all atomic types at once see
[opts_atomic](https://cynkra.github.io/constructive/reference/opts_atomic.md)().

## Usage

``` r
opts_raw(
  constructor = c("as.raw", "charToRaw"),
  ...,
  trim = NULL,
  fill = c("default", "rlang", "+", "...", "none"),
  compress = TRUE,
  representation = c("hexadecimal", "decimal")
)
```

## Arguments

- constructor:

  String. Name of the function used to construct the object.

- ...:

  Additional options used by user defined constructors through the
  `opts` object

- trim:

  `NULL` or integerish. Maximum of elements showed before it's trimmed.
  Note that it will necessarily produce code that doesn't reproduce the
  input. This code will parse without failure but its evaluation might
  fail.

- fill:

  String. Method to use to represent the trimmed elements. See
  [`?opts_atomic`](https://cynkra.github.io/constructive/reference/opts_atomic.md)

- compress:

  Boolean. If `TRUE` instead of [`c()`](https://rdrr.io/r/base/c.html)
  Use [`seq()`](https://rdrr.io/r/base/seq.html),
  [`rep()`](https://rdrr.io/r/base/rep.html) when relevant to simplify
  the output.

- representation:

  For "as.raw" constructor. Respectively generate output in the formats
  `as.raw(0x10)` or `as.raw(16)`

## Value

An object of class \<constructive_options/constructive_options_raw\>
