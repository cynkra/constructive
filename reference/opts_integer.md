# Constructive options for type 'integer'

These options will be used on objects of type 'integer'. This type has a
single native constructor, but some additional options can be set.

To set options on all atomic types at once see
[opts_atomic](https://cynkra.github.io/constructive/reference/opts_atomic.md)().

## Usage

``` r
opts_integer(
  constructor = c("default"),
  ...,
  trim = NULL,
  fill = c("default", "rlang", "+", "...", "none"),
  compress = TRUE
)
```

## Arguments

- constructor:

  String. Method used to construct the object, often the name of a
  function.

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

## Value

An object of class \<constructive_options/constructive_options_integer\>
